{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module RunGhcBWrap
  ( runHaskellFilesInSandbox
  , runSandboxedExecutable
  , runHaskellInSandbox
  , runHaskellInTimedSandbox
  , testRunGhcBWrap
  , splitOnNewline
  , splitOnBar
  , RunGhcError(..)
  ) where

import RunGhc.Executable
import RunGhc.LocatedModule
import RunGhc.Locate
import RunGhc.UserInput (RunGhcError(..))

import System.Which
import System.Process as P
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>), takeDirectory)
import System.Exit (ExitCode(..))
import System.Timeout
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException, displayException)
import System.Environment (getEnv, getEnvironment)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)

import Control.Monad
import Data.List (intercalate)
import qualified Data.Text as T 

-- runghc = $(staticWhich "runghc")
-- ghc = $(staticWhich "ghc")
runghc912 = $(staticWhich "runghc-9.12.2")
ghc912 = $(staticWhich "ghc-9.12.2")
bubblewrap = $(staticWhich "bwrap")
ghcPkg912 = $(staticWhich "ghc-pkg-9.12.2")
ghcPkg = $(staticWhich "ghc-pkg")

nixShell = $(staticWhich "nix-shell")
-- Current Result:
--(ExitFailure 1,"","warning: '/nix' does not exist, so Nix will use '/homeless-shelter/.local/share/nix/root' as a chroot store\nerror: cannot figure out user name\n")

-- nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ temporary vector aeson ])" bubblewrap cabal-install which --run "runghc TestBWrap.hs" --pure

testRunGhcBWrap :: String -> IO ()
testRunGhcBWrap script = do
  putStrLn $ "With runghc: " <> runghc912
  --print (runghc912, runghc, ghc, bubblewrap)
  x <- runHaskellInSandbox (script, "")

  case x of
    Left e -> do
      forM_ (splitOnNewline $ displayException e) putStrLn
    Right (exitCode, stdout, stderr) -> do
      print exitCode
      forM_ (splitOnNewline stdout) $ putStrLn
      forM_ (splitOnNewline stderr) putStrLn
  pure ()


splitOnNewline :: String -> [String]
splitOnNewline [] = [""]
splitOnNewline s  =
  let (line, rest) = break (== '\n') s
  in line : case rest of
              []      -> []
              (_:xs)  -> splitOnNewline xs
splitOnBar :: String -> [String]
splitOnBar [] = [""]
splitOnBar s  =
  let (line, rest) = break (== '|') s
  in line : case rest of
              []      -> []
              (_:xs)  -> splitOnNewline xs


-- nix-shell -p "haskellPackages.ghcWithPackages (ps: [ ps.temporary ])" bubblewrap cabal-install --run "runghc TestBWrap.hs"

-- | Run Haskell source code in a time-limited, sandboxed environment
-- | TODO: benchmark user's code, stream edit (main = -> main')
-- | and input (main = withTimer main') 
-- runHaskellInTimedSandbox
--   :: Int
--   -- ^ Number of microseconds to allow
--   -> (String, String)
--   -> IO (Maybe (Either SomeException (ExitCode, String, String)))
-- runHaskellFilesInTimedSandbox timeAllowed inputs =
--   timeout timeAllowed $ runHaskellFilesInSandbox inputs


-- | Run Haskell source code in a time-limited, sandboxed environment
-- | TODO: benchmark user's code, stream edit (main = -> main')
-- | and input (main = withTimer main') 
runHaskellInTimedSandbox
  :: Int
  -- ^ Number of microseconds to allow
  -> (String, String)
  -> IO (Maybe (Either SomeException (ExitCode, String, String)))
runHaskellInTimedSandbox timeAllowed inputs =
  timeout timeAllowed $ runHaskellInSandbox inputs


-- | Common bwrap sandbox arguments
bwrapBaseArgs :: FilePath -> FilePath -> String -> String -> [String]
bwrapBaseArgs projectDir tmpBindDir hostPath globalPkgDb =
  [ "--die-with-parent"
  , "--bind", projectDir, "/project"
  , "--bind", tmpBindDir, "/tmp"
  , "--dev", "/dev"
  , "--proc", "/proc"
  , "--ro-bind", "/nix/store", "/nix/store"
  , "--setenv", "PATH", hostPath
  , "--setenv", "TMPDIR", "/tmp"
  , "--setenv", "GHC_PACKAGE_PATH", globalPkgDb
  , "--chdir", "/project"
  ]

-- | Compile a single .hs file with ghc -c inside bwrap
bwrapGhcCompile :: FilePath -> FilePath -> String -> String -> FilePath -> IO (ExitCode, String, String)
bwrapGhcCompile projectDir tmpBindDir hostPath globalPkgDb hsPath =
  readCreateProcessWithExitCode
    (P.proc bubblewrap $ bwrapBaseArgs projectDir tmpBindDir hostPath globalPkgDb
      ++ [ghc912, "-c", hsPath])
    ""

-- | Link .o files into an executable inside bwrap
bwrapGhcLink :: FilePath -> FilePath -> String -> String -> [FilePath] -> FilePath -> IO (ExitCode, String, String)
bwrapGhcLink projectDir tmpBindDir hostPath globalPkgDb oFiles outputName =
  readCreateProcessWithExitCode
    (P.proc bubblewrap $ bwrapBaseArgs projectDir tmpBindDir hostPath globalPkgDb
      ++ [ghc912] ++ oFiles ++ ["-o", outputName])
    ""

-- | Run a compiled binary inside bwrap
bwrapRunBinary :: FilePath -> FilePath -> String -> String -> FilePath -> String -> IO (ExitCode, String, String)
bwrapRunBinary projectDir tmpBindDir hostPath globalPkgDb binaryPath stdinStr =
  readCreateProcessWithExitCode
    (P.proc bubblewrap $ bwrapBaseArgs projectDir tmpBindDir hostPath globalPkgDb
      ++ [binaryPath])
    stdinStr

runHaskellFilesInSandbox
  :: (Executable, String) -> IO (Either SomeException (ExitCode, String, String))
runHaskellFilesInSandbox (exe, stdin) = try $ do
  let testModule = _main exe
  let sourceFiles = _library exe
  let folders = takeDirectory . pathSegsToPath ".hs" . getPathSegments <$> (testModule : sourceFiles)

  -- trim newline char
  globalPkgDb <- fmap init $ readProcess ghc912 ["--print-global-package-db"] ""

  withSystemTempDirectory "sandbox" $ \tmpDir -> do
    let tmpBindDir = tmpDir </> "tmp"
    createDirectoryIfMissing True tmpBindDir
    let projectDir = tmpDir </> "project"
    let baseDir = projectDir
    let expectedMainFile = "Main.hs"

    putStrLn $ "StdIn: " <> stdin
    forM_ folders $ \fldr -> do
      createDirectoryIfMissing True (baseDir </> fldr)
    writeLocatedFiles baseDir (_main exe : _library exe)
    print =<< listDirectory projectDir
    hostPath <- getEnv "PATH"
    let bwrapCmd = P.proc bubblewrap $
          [ "--die-with-parent"
          , "--bind", projectDir, "/project"
          , "--bind", tmpBindDir, "/tmp"
          , "--dev", "/dev"
          , "--proc", "/proc"
          , "--ro-bind", "/nix/store", "/nix/store"
          , "--setenv", "PATH", hostPath
          , "--setenv", "TMPDIR", "/tmp"
          , "--setenv", "GHC_PACKAGE_PATH", globalPkgDb
          , "--chdir", "/project"
          , runghc912, "-f", ghc912, expectedMainFile
          ] <> words stdin
    readCreateProcessWithExitCode bwrapCmd ""

-- | Run a SandboxedExecutable with two-phase compilation to prevent
-- Template Haskell sandbox escapes.
--
-- Phase 1: Compile untrusted (user) modules in isolation — TH runs here
--          but no secret/trusted files exist yet
-- Phase 2: Write trusted modules + Main
-- Phase 3: Compile trusted modules + link + run
runSandboxedExecutable
  :: (SandboxedExecutable, String) -> IO (Either SomeException (Either RunGhcError (ExitCode, String, String)))
runSandboxedExecutable (sandboxed, stdinStr) = try $ runExceptT $ do
  let exe = _sandboxedExe sandboxed
  let untrusted = _untrustedModules sandboxed
  let trusted = _library exe
  let allModules = _main exe : untrusted ++ trusted
  let allFolders = takeDirectory . pathSegsToPath ".hs" . getPathSegments <$> allModules

  -- Get the ghcWithPackages package db path. This is a single combined db
  -- in /nix/store with all packages (aeson, runGhcBWrap-core, etc).
  -- We must set GHC_PACKAGE_PATH explicitly in bwrap because the parent
  -- env may have GHC_PACKAGE_PATH pointing to a nix build sandbox path
  -- (e.g. /build/tmp.xxx/) that isn't mounted inside bwrap.
  -- Clear GHC_PACKAGE_PATH before running ghc-pkg so it doesn't inherit
  -- the parent env's GHC 8.10 package db (incompatible format with 9.12)
  env <- liftIO getEnvironment
  let env' = filter ((/= "GHC_PACKAGE_PATH") . fst) env
  pkgListOutput <- liftIO $ readCreateProcess
    (P.proc ghcPkg912 ["list"]) { env = Just env' } ""
  let pkgDbs = [line | line <- lines pkgListOutput, not (null line), safeHead line == Just '/']
  let ghcPackagePath = intercalate ":" pkgDbs

  ExceptT $ withSystemTempDirectory "sandbox" $ \tmpDir -> runExceptT $ do
    let tmpBindDir = tmpDir </> "tmp"
    liftIO $ createDirectoryIfMissing True tmpBindDir
    let projectDir = tmpDir </> "project"
    let baseDir = projectDir
    hostPath <- liftIO $ getEnv "PATH"

    let sandboxArgs =
          [ "--die-with-parent"
          , "--bind", projectDir, "/project"
          , "--bind", tmpBindDir, "/tmp"
          , "--dev", "/dev"
          , "--proc", "/proc"
          , "--ro-bind", "/nix/store", "/nix/store"
          , "--setenv", "PATH", hostPath
          , "--setenv", "TMPDIR", "/tmp"
          , "--setenv", "GHC_PACKAGE_PATH", ghcPackagePath
          , "--chdir", "/project"
          ]

    -- Create directory structure for all modules upfront
    liftIO $ forM_ allFolders $ \fldr ->
      createDirectoryIfMissing True (baseDir </> fldr)

    -- Phase 1: Write and compile untrusted modules in isolation.
    -- TH splices run here but there are no secret/trusted files to read.
    -- Source is kept (not deleted) so ghc --make can find modules later,
    -- but .o is newer than .hs so ghc --make will skip recompilation
    -- (TH does NOT re-execute).
    liftIO $ writeLocatedFiles baseDir untrusted
    forM_ untrusted $ \m -> do
      let hsPath = pathSegsToPath ".hs" (getPathSegments m)
      (ec, _, err) <- liftIO $ readCreateProcessWithExitCode
        (P.proc bubblewrap $ sandboxArgs ++ [ghc912, "-c", hsPath]) ""
      when (ec /= ExitSuccess) $
        throwE $ Stage1Error_ReadUntrusted $ T.pack err
        -- T.pack $ "Phase 1 compilation failed for " ++ hsPath ++ ": " ++ err

    -- Phase 2: Write trusted modules + Main
    liftIO $ writeLocatedFiles baseDir [_main exe]
    liftIO $ writeLocatedFiles baseDir trusted

    -- Phase 3: Compile trusted sources + link in one step.
    -- ghc --make chases imports from Main.hs, compiles trusted .hs sources,
    -- skips untrusted modules (already compiled, .o newer than .hs),
    -- and links with the correct package libraries since it reads
    -- package deps from .hi files.
    let mainHsPath = pathSegsToPath ".hs" (getPathSegments (_main exe))
    (ec, _, err) <- liftIO $ readCreateProcessWithExitCode
      (P.proc bubblewrap $ sandboxArgs ++ [ghc912, "--make", mainHsPath, "-o", "Main"]) ""
    when (ec /= ExitSuccess) $
      throwE $ Stage2Error_Link $ T.pack err

    -- Phase 3.5: Delete all .hs source files so they cannot be read at runtime
    liftIO $ forM_ allModules $ \m ->
      removeFile (baseDir </> pathSegsToPath ".hs" (getPathSegments m))

    -- Phase 4: Run the binary
    liftIO $ readCreateProcessWithExitCode
      (P.proc bubblewrap $ sandboxArgs ++ ["./Main"]) stdinStr



-- runghc
--
-- User Input
-- """
-- module Hask where

-- f :: Int -> Int -> Int
-- """

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Lift an IO action into ExceptT, catching SomeException as RunGhcError
-- tryIO :: IO a -> ExceptT RunGhcError IO a
-- tryIO act = do
--   r <- liftIO $ try act
--   case r of
--     Left (e :: SomeException) -> throwE $ RunGhcError $ T.pack $ displayException e
--     Right a -> pure a

-- data RunGhcError 
--   = ParseError
--   | TypeError T.Text
--   | NoInstanceFor T.Text -- ~type error
--   | UnsupportedExtension T.Text
--   | Unrecognized SomeException
  


-- | Run Haskell source code in a sandboxed environment
runHaskellInSandbox :: (String, String) -> IO (Either SomeException (ExitCode, String, String))
runHaskellInSandbox (sourceCode, stdin) = try $ do
  -- trim newline char
  globalPkgDb <- fmap init $ readProcess ghc912 ["--print-global-package-db"] ""

  withSystemTempDirectory "sandbox" $ \tmpDir -> do
    let projectDir = tmpDir </> "project"
    let tmpBindDir = tmpDir </> "tmp"
    putStrLn $ "StdIn: " <> stdin
    createDirectoryIfMissing True projectDir
    createDirectoryIfMissing True tmpBindDir
    createDirectoryIfMissing True $ projectDir </> "src"
    let hsFile = projectDir </> "src" </> "Main.hs"
    writeFile hsFile sourceCode
    hostPath <- getEnv "PATH"
    let bwrapCmd = P.proc bubblewrap $
          [ "--bind", projectDir, "/project"
          , "--bind", tmpBindDir, "/tmp"
          , "--dev", "/dev"
          , "--proc", "/proc"
          , "--ro-bind", "/nix/store", "/nix/store"
          , "--setenv", "PATH", hostPath
          , "--setenv", "TMPDIR", "/tmp"
          , "--setenv", "GHC_PACKAGE_PATH", globalPkgDb
          , "--chdir", "/project"
          , runghc912, "-f", ghc912, "src/Main.hs"
          ] <> words stdin
    readCreateProcessWithExitCode bwrapCmd ""
