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
import System.Directory (createDirectoryIfMissing, removeFile)

import Control.Monad

-- runghc = $(staticWhich "runghc")
-- ghc = $(staticWhich "ghc")
runghc912 = $(staticWhich "runghc-9.12.2")
ghc912 = $(staticWhich "ghc-9.12.2")
bubblewrap = $(staticWhich "bwrap")


nix = $(staticWhich "nix-shell")

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
bwrapBaseArgs :: FilePath -> FilePath -> String -> [String]
bwrapBaseArgs projectDir tmpBindDir hostPath =
  [ "--bind", projectDir, "/project"
  , "--bind", tmpBindDir, "/tmp"
  , "--dev", "/dev"
  , "--proc", "/proc"
  , "--ro-bind", "/nix/store", "/nix/store"
  , "--setenv", "PATH", hostPath
  , "--setenv", "TMPDIR", "/tmp"
  , "--chdir", "/project"
  ]

-- | Compile a single .hs file with ghc -c inside bwrap
bwrapGhcCompile :: FilePath -> FilePath -> String -> FilePath -> IO (ExitCode, String, String)
bwrapGhcCompile projectDir tmpBindDir hostPath hsPath =
  readCreateProcessWithExitCode
    (P.proc bubblewrap $ bwrapBaseArgs projectDir tmpBindDir hostPath
      ++ [ghc912, "-c", hsPath])
    ""

-- | Link .o files into an executable inside bwrap
bwrapGhcLink :: FilePath -> FilePath -> String -> [FilePath] -> FilePath -> IO (ExitCode, String, String)
bwrapGhcLink projectDir tmpBindDir hostPath oFiles outputName =
  readCreateProcessWithExitCode
    (P.proc bubblewrap $ bwrapBaseArgs projectDir tmpBindDir hostPath
      ++ [ghc912] ++ oFiles ++ ["-o", outputName])
    ""

-- | Run a compiled binary inside bwrap
bwrapRunBinary :: FilePath -> FilePath -> String -> FilePath -> String -> IO (ExitCode, String, String)
bwrapRunBinary projectDir tmpBindDir hostPath binaryPath stdinStr =
  readCreateProcessWithExitCode
    (P.proc bubblewrap $ bwrapBaseArgs projectDir tmpBindDir hostPath
      ++ [binaryPath])
    stdinStr

-- | Run an Executable in a sandboxed environment.
-- No TH isolation — all modules are compiled together.
-- Use 'runSandboxedExecutable' for untrusted code.
runHaskellFilesInSandbox
  :: (Executable, String) -> IO (Either SomeException (ExitCode, String, String))
runHaskellFilesInSandbox (exe, stdinStr) = try $ do
  let allModules = _main exe : _library exe
  let allFolders = takeDirectory . pathSegsToPath ".hs" . getPathSegments <$> allModules
  withSystemTempDirectory "sandbox" $ \tmpDir -> do
    let tmpBindDir = tmpDir </> "tmp"
    createDirectoryIfMissing True tmpBindDir
    let projectDir = tmpDir </> "project"
    let baseDir = projectDir
    hostPath <- getEnv "PATH"

    forM_ allFolders $ \fldr ->
      createDirectoryIfMissing True (baseDir </> fldr)
    writeLocatedFiles baseDir allModules

    -- Compile library modules first, then Main
    forM_ (_library exe ++ [_main exe]) $ \m -> do
      let hsPath = pathSegsToPath ".hs" (getPathSegments m)
      (ec, _, err) <- bwrapGhcCompile projectDir tmpBindDir hostPath hsPath
      when (ec /= ExitSuccess) $
        fail $ "Compilation failed for " ++ hsPath ++ ": " ++ err

    -- Link
    let allOFiles = [ pathSegsToPath ".o" (getPathSegments m) | m <- allModules ]
    (ec, _, err) <- bwrapGhcLink projectDir tmpBindDir hostPath allOFiles "Main"
    when (ec /= ExitSuccess) $
      fail $ "Linking failed: " ++ err

    -- Run
    bwrapRunBinary projectDir tmpBindDir hostPath "./Main" stdinStr

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
  withSystemTempDirectory "sandbox" $ \tmpDir -> do
    let tmpBindDir = tmpDir </> "tmp"
    createDirectoryIfMissing True tmpBindDir
    let projectDir = tmpDir </> "project"
    let baseDir = projectDir
    hostPath <- getEnv "PATH"

    -- Create directory structure for all modules upfront
    forM_ allFolders $ \fldr ->
      createDirectoryIfMissing True (baseDir </> fldr)

    -- Phase 1: Write and compile untrusted modules in isolation
    -- TH splices run here but there are no secret files to read
    writeLocatedFiles baseDir untrusted
    forM_ untrusted $ \m -> do
      let hsPath = pathSegsToPath ".hs" (getPathSegments m)
      (ec, _, err) <- bwrapGhcCompile projectDir tmpBindDir hostPath hsPath
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
      (ec, _, err) <- bwrapGhcCompile projectDir tmpBindDir hostPath hsPath
      when (ec /= ExitSuccess) $
        fail $ "Phase 3 compilation failed for " ++ hsPath ++ ": " ++ err

    -- Phase 4: Link all .o files into binary
    let allOFiles = [ pathSegsToPath ".o" (getPathSegments m) | m <- allModules ]
    (ec, _, err) <- bwrapGhcLink projectDir tmpBindDir hostPath allOFiles "Main"
    when (ec /= ExitSuccess) $
      fail $ "Linking failed: " ++ err

    -- Phase 5: Run the binary
    bwrapRunBinary projectDir tmpBindDir hostPath "./Main" stdinStr

-- | Run Haskell source code in a sandboxed environment (single source string)
runHaskellInSandbox :: (String, String) -> IO (Either SomeException (ExitCode, String, String))
runHaskellInSandbox (sourceCode, stdinStr) = try $ do
  withSystemTempDirectory "sandbox" $ \tmpDir -> do
    let projectDir = tmpDir </> "project"
    let tmpBindDir = tmpDir </> "tmp"
    createDirectoryIfMissing True projectDir
    createDirectoryIfMissing True tmpBindDir
    let hsFile = projectDir </> "Main.hs"
    writeFile hsFile sourceCode
    hostPath <- getEnv "PATH"
    -- Compile
    (ec, _, err) <- bwrapGhcCompile projectDir tmpBindDir hostPath "Main.hs"
    when (ec /= ExitSuccess) $
      fail $ "Compilation failed: " ++ err
    -- Link
    (ec2, _, err2) <- bwrapGhcLink projectDir tmpBindDir hostPath ["Main.o"] "Main"
    when (ec2 /= ExitSuccess) $
      fail $ "Linking failed: " ++ err2
    -- Run
    bwrapRunBinary projectDir tmpBindDir hostPath "./Main" stdinStr
