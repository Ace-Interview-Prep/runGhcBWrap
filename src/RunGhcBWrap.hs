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
  ) where

import RunGhc.Executable
import RunGhc.LocatedModule
import RunGhc.Locate

import System.Which
import System.Process as P
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>), takeDirectory)
import System.Exit (ExitCode(..))
import System.Timeout
import Control.Exception (try, SomeException, displayException)
import System.Environment (getEnv)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)

import Control.Monad
import Data.List (intercalate)

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
  [ "--bind", projectDir, "/project"
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
          [ "--bind", projectDir, "/project"
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
  :: (SandboxedExecutable, String) -> IO (Either SomeException (ExitCode, String, String))
runSandboxedExecutable (sandboxed, stdinStr) = try $ do
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
  pkgListOutput <- readProcess ghcPkg912 ["list"] ""
  let pkgDbs = [line | line <- lines pkgListOutput, not (null line), head line == '/']
  let ghcPackagePath = intercalate ":" pkgDbs

  withSystemTempDirectory "sandbox" $ \tmpDir -> do
    let tmpBindDir = tmpDir </> "tmp"
    createDirectoryIfMissing True tmpBindDir
    let projectDir = tmpDir </> "project"
    let baseDir = projectDir
    hostPath <- getEnv "PATH"

    let sandboxArgs =
          [ "--bind", projectDir, "/project"
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
    forM_ allFolders $ \fldr ->
      createDirectoryIfMissing True (baseDir </> fldr)

    -- Phase 1: Write and compile untrusted modules in isolation
    -- TH splices run here but there are no secret files to read
    writeLocatedFiles baseDir untrusted
    forM_ untrusted $ \m -> do
      let hsPath = pathSegsToPath ".hs" (getPathSegments m)
      (ec, _, err) <- readCreateProcessWithExitCode
        (P.proc bubblewrap $ sandboxArgs ++ [ghc912, "-c", hsPath]) ""
      -- Delete source after compilation, keep .o/.hi
      removeFile (baseDir </> hsPath)
      when (ec /= ExitSuccess) $
        fail $ "Phase 1 compilation failed for " ++ hsPath ++ ": " ++ err

    -- Phase 2: Write trusted modules + Main
    writeLocatedFiles baseDir [_main exe]
    writeLocatedFiles baseDir trusted

    -- Phase 3: Compile trusted modules first, then Main (one-shot, finds untrusted .hi)
    forM_ (trusted ++ [_main exe]) $ \m -> do
      let hsPath = pathSegsToPath ".hs" (getPathSegments m)
      (ec, _, err) <- readCreateProcessWithExitCode
        (P.proc bubblewrap $ sandboxArgs ++ [ghc912, "-c", hsPath]) ""
      when (ec /= ExitSuccess) $
        fail $ "Phase 3 compilation failed for " ++ hsPath ++ ": " ++ err

    -- Phase 4: Link all .o files into binary
    let allOFiles = [ pathSegsToPath ".o" (getPathSegments m) | m <- allModules ]
    (ec, _, err) <- readCreateProcessWithExitCode
      (P.proc bubblewrap $ sandboxArgs ++ [ghc912] ++ allOFiles ++ ["-o", "Main"]) ""
    when (ec /= ExitSuccess) $
      fail $ "Linking failed: " ++ err

    -- Phase 5: Run the binary
    readCreateProcessWithExitCode
      (P.proc bubblewrap $ sandboxArgs ++ ["./Main"]) stdinStr

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
