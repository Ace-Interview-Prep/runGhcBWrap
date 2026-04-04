{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import RunGhcBWrap
import RunGhc.Executable
import RunGhc.LocatedModule
import RunGhc.Locate
import RunGhc.SystemModule

import System.Exit (ExitCode(..))
import Data.List (isInfixOf)
import qualified Data.Text as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "runGhcBWrap"
  [ testGroup "runSandboxedExecutable"
      [ testCase "legitimate code runs correctly" testLegitimateCode
      , testCase "malicious TH readFile cannot read secret" testMaliciousTHReadFile
      , testCase "TH listDirectory cannot see trusted modules" testTHListDirectory
      , testCase "TH enumerate and dump all files cannot see secret" testTHEnumerateAndDump
      , testCase "multiple user modules compiled in isolation" testMultipleUserModules
      , testCase "exit code propagated from runtime error" testExitCodePropagation
      , testCase "trusted code using external packages (aeson) links correctly" testExternalPackageLinking
      , testCase "trusted code importing runGhcBWrap-core (TryCodeResult)" testRunGhcBWrapCoreImport
      ]
  , testGroup "runHaskellFilesInSandbox"
      [ testCase "trusted executable runs correctly" testTrustedExecutable
      , testCase "Executable type IS vulnerable to TH enumerate+dump" testExecutableVulnerable
      ]
  , testGroup "runHaskellInSandbox"
      [ testCase "simple hello world" testSimpleHelloWorld
      ]
  ]

-- Helper: create a user-submitted module (untrusted)
mkUserModule :: [String] -> String -> LocatedModule
mkUserModule pathSegs source =
  FromLocatedScript $ LocatedScript
    (map (PathSegment . T.pack) pathSegs)
    (Script (T.pack source))

-- Helper: create a system-generated module (trusted)
mkSystemModule :: [String] -> String -> LocatedModule
mkSystemModule pathSegs expr =
  FromSystemModule
    (map (PathSegment . T.pack) pathSegs)
    (ExpressionsOnly (Expressions (T.pack expr)))

-- Helper: create Main module (system-generated)
mkMainModule :: String -> LocatedModule
mkMainModule expr = mkSystemModule ["Main"] expr

-- Helper: build a SandboxedExecutable
mkSandboxed :: LocatedModule -> [LocatedModule] -> [LocatedModule] -> SandboxedExecutable
mkSandboxed mainMod userMods trustedMods = SandboxedExecutable
  { _sandboxedExe = Executable { _main = mainMod, _library = trustedMods }
  , _untrustedModules = userMods
  }

-- Test: legitimate user code with no TH runs correctly
testLegitimateCode :: Assertion
testLegitimateCode = do
  let userMod = mkUserModule ["UserModule"]
        "module UserModule where\ngreet :: String -> String\ngreet name = \"Hello, \" ++ name ++ \"!\""
  let secretMod = mkSystemModule ["SecretSolution"]
        "answer :: Int -> String\nanswer n = \"secret:\" ++ show n"
  let mainMod = mkMainModule
        "import UserModule\nimport SecretSolution\nmain :: IO ()\nmain = do\n  putStrLn (greet \"world\")\n  putStrLn (answer 42)"
  let sandboxed = mkSandboxed mainMod [userMod] [secretMod]
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> assertFailure $ "Unexpected error: " ++ show err
    Right (Right (ec, stdout, _stderr)) -> do
      ec @?= ExitSuccess
      assertBool "stdout contains greeting" ("Hello, world!" `isInfixOf` stdout)
      assertBool "stdout contains answer" ("secret:42" `isInfixOf` stdout)

-- Test: malicious TH that tries to readFile a secret module
-- Phase 1 compilation should fail because the secret doesn't exist yet
testMaliciousTHReadFile :: Assertion
testMaliciousTHReadFile = do
  let maliciousMod = mkUserModule ["UserModule"]
        "{-# LANGUAGE TemplateHaskell #-}\nmodule UserModule where\nimport Control.Monad.IO.Class\nmyFunc :: String\nmyFunc = $(liftIO (readFile \"SecretSolution.hs\" >>= putStrLn) *> [| \"\" |])"
  let secretMod = mkSystemModule ["SecretSolution"]
        "answer :: Int -> String\nanswer n = \"the secret is \" ++ show n"
  let mainMod = mkMainModule
        "import UserModule\nimport SecretSolution\nmain :: IO ()\nmain = putStrLn (answer 42)"
  let sandboxed = mkSandboxed mainMod [maliciousMod] [secretMod]
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> do
      let errMsg = show err
      assertBool "error mentions compilation failure" ("Stage1Error_ReadUntrusted" `isInfixOf` errMsg)
    Right (Right (_ec, stdout, stderr)) -> do
      assertBool "secret not leaked in stdout" (not $ "the secret is" `isInfixOf` stdout)
      assertBool "secret not leaked in stderr" (not $ "the secret is" `isInfixOf` stderr)

-- Test: TH listDirectory during Phase 1 cannot see trusted modules
-- User module uses TH to list project directory at compile time,
-- embedding the result as a string. At runtime we verify trusted
-- module files (SecretSolution.hs, Main.hs) are NOT visible.
testTHListDirectory :: Assertion
testTHListDirectory = do
  let userMod = mkUserModule ["UserModule"] $ unlines
        [ "{-# LANGUAGE TemplateHaskell #-}"
        , "module UserModule where"
        , "import Language.Haskell.TH"
        , "import System.Directory (listDirectory)"
        , "visibleFiles :: String"
        , "visibleFiles = $(do"
        , "  files <- runIO (listDirectory \".\")"
        , "  litE (stringL (show files)))"
        ]
  let secretMod = mkSystemModule ["SecretSolution"]
        "answer :: String\nanswer = \"top secret\""
  let mainMod = mkMainModule
        "import UserModule\nmain :: IO ()\nmain = putStrLn visibleFiles"
  let sandboxed = mkSandboxed mainMod [userMod] [secretMod]
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> assertFailure $ "Unexpected error: " ++ show err
    Right (Right (ec, stdout, _stderr)) -> do
      ec @?= ExitSuccess
      assertBool "SecretSolution.hs not visible during Phase 1"
        (not $ "SecretSolution" `isInfixOf` stdout)
      assertBool "Main.hs not visible during Phase 1"
        (not $ "Main" `isInfixOf` stdout)

-- Test: TH enumerates all files and reads+prints their contents at compile time.
-- This is the most aggressive exfiltration attack: listDirectory then readFile everything.
-- Phase 1 isolation means only the user's own module exists, so no secrets leak.
testTHEnumerateAndDump :: Assertion
testTHEnumerateAndDump = do
  let maliciousMod = mkUserModule ["UserModule"] $ unlines
        [ "{-# LANGUAGE TemplateHaskell #-}"
        , "module UserModule where"
        , "import Control.Monad.IO.Class"
        , "import Data.Foldable"
        , "import System.Directory"
        , "funName :: String -> String"
        , "funName = $(liftIO (listDirectory \".\" >>= traverse_ (\\x -> putStrLn (x ++ \":\") *> (readFile x >>= print))) *> [| const \"\" |])"
        ]
  let secretMod = mkSystemModule ["SecretSolution"]
        "answer :: String\nanswer = \"the secret is 42\""
  let mainMod = mkMainModule
        "import UserModule\nimport SecretSolution\nmain :: IO ()\nmain = putStrLn (funName answer)"
  let sandboxed = mkSandboxed mainMod [maliciousMod] [secretMod]
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> do
      let errMsg = show err
      -- If it fails, the secret must not be in the error message
      assertBool "secret not leaked in error" (not $ "the secret is 42" `isInfixOf` errMsg)
    Right (Right (_ec, stdout, stderr)) -> do
      assertBool "secret not leaked in stdout" (not $ "the secret is 42" `isInfixOf` stdout)
      assertBool "secret not leaked in stderr" (not $ "the secret is 42" `isInfixOf` stderr)

-- Test: multiple user modules are all compiled in isolation
testMultipleUserModules :: Assertion
testMultipleUserModules = do
  let userMod1 = mkUserModule ["ModA"]
        "module ModA where\nfuncA :: Int\nfuncA = 10"
  let userMod2 = mkUserModule ["ModB"]
        "module ModB where\nfuncB :: Int\nfuncB = 20"
  let secretMod = mkSystemModule ["SecretSolution"]
        "answer :: Int -> Int -> String\nanswer a b = \"result:\" ++ show (a + b)"
  let mainMod = mkMainModule
        "import ModA\nimport ModB\nimport SecretSolution\nmain :: IO ()\nmain = putStrLn (answer funcA funcB)"
  let sandboxed = mkSandboxed mainMod [userMod1, userMod2] [secretMod]
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> assertFailure $ "Unexpected error: " ++ show err
    Right (Right (ec, stdout, _stderr)) -> do
      ec @?= ExitSuccess
      assertBool "correct result" ("result:30" `isInfixOf` stdout)

-- Test: runtime errors propagate proper exit code
testExitCodePropagation :: Assertion
testExitCodePropagation = do
  let userMod = mkUserModule ["UserModule"]
        "module UserModule where\nboom :: String\nboom = error \"kaboom\""
  let mainMod = mkMainModule
        "import UserModule\nmain :: IO ()\nmain = putStrLn boom"
  let sandboxed = mkSandboxed mainMod [userMod] []
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> assertFailure $ "Unexpected error: " ++ show err
    Right (Right (ec, _stdout, stderr)) -> do
      assertBool "non-zero exit code" (ec /= ExitSuccess)
      assertBool "stderr mentions error" ("kaboom" `isInfixOf` stderr)

-- Test: trusted Main uses external packages (aeson, bytestring).
-- This reproduces the backend-codechallenges linker failure:
-- ghc -c finds the packages fine, but ghc .o -o Main doesn't know
-- which packages to link against.
testExternalPackageLinking :: Assertion
testExternalPackageLinking = do
  let userMod = mkUserModule ["UserModule"]
        "module UserModule where\nresult :: (Int, Int)\nresult = (1, 2)"
  let mainMod = mkMainModule $ unlines
        [ "import UserModule"
        , "import Data.Aeson (encode)"
        , "import qualified Data.ByteString.Lazy.Char8 as BL"
        , "main :: IO ()"
        , "main = BL.putStrLn (encode result)"
        ]
  let sandboxed = mkSandboxed mainMod [userMod] []
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> assertFailure $ "Unexpected error: " ++ show err
    Right (Right (ec, stdout, _stderr)) -> do
      ec @?= ExitSuccess
      assertBool "output contains encoded JSON" ("[1,2]" `isInfixOf` stdout)

-- Test: both untrusted (Phase 1) and trusted (Phase 3) can import
-- runGhcBWrap-core modules. User module uses TryCodeResult in Phase 1,
-- trusted Main uses it in Phase 3 with aeson encode.
testRunGhcBWrapCoreImport :: Assertion
testRunGhcBWrapCoreImport = do
  let userMod = mkUserModule ["UserModule"] $ unlines
        [ "module UserModule where"
        , "import RunGhc.MakeTest (TryCodeResult(..))"
        , "mkResult :: Int -> Int -> TryCodeResult Int Int"
        , "mkResult a b = TryCodeResult a b b (a == b)"
        ]
  let mainMod = mkMainModule $ unlines
        [ "import UserModule"
        , "import RunGhc.MakeTest (TryCodeResult(..))"
        , "import Data.Aeson (encode)"
        , "import qualified Data.ByteString.Lazy.Char8 as BL"
        , "main :: IO ()"
        , "main = BL.putStrLn (encode (mkResult 1 2))"
        ]
  let sandboxed = mkSandboxed mainMod [userMod] []
  result <- runSandboxedExecutable (sandboxed, "")
  case result of
    Left exc -> assertFailure $ "Unexpected exception: " ++ show exc
    Right (Left err) -> assertFailure $ "Unexpected error: " ++ show err
    Right (Right (ec, stdout, _stderr)) -> do
      ec @?= ExitSuccess
      assertBool "output contains TryCodeResult JSON" ("_success" `isInfixOf` stdout)

-- Test: plain Executable (no untrusted separation) still works
testTrustedExecutable :: Assertion
testTrustedExecutable = do
  let mainMod = mkSystemModule ["Main"]
        "main :: IO ()\nmain = putStrLn \"all trusted\""
  let exe = Executable { _main = mainMod, _library = [] }
  result <- runHaskellFilesInSandbox (exe, "")
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right (ec, stdout, _stderr) -> do
      ec @?= ExitSuccess
      assertBool "stdout correct" ("all trusted" `isInfixOf` stdout)

-- Test: the same TH enumerate+dump attack SUCCEEDS with plain Executable,
-- proving the vulnerability exists when not using SandboxedExecutable.
-- All files are written before runghc runs, so TH can read everything.
testExecutableVulnerable :: Assertion
testExecutableVulnerable = do
  let maliciousMod = mkUserModule ["UserModule"] $ unlines
        [ "{-# LANGUAGE TemplateHaskell #-}"
        , "module UserModule where"
        , "import Control.Monad.IO.Class"
        , "import Data.Foldable"
        , "import System.Directory"
        , "funName :: String -> String"
        , "funName = $(liftIO (listDirectory \".\" >>= traverse_ (\\x -> putStrLn (x ++ \":\") *> (readFile x >>= print))) *> [| const \"\" |])"
        ]
  let secretMod = mkSystemModule ["SecretSolution"]
        "answer :: String\nanswer = \"the secret is 42\""
  let mainMod = mkSystemModule ["Main"]
        "import UserModule\nimport SecretSolution\nmain :: IO ()\nmain = putStrLn (funName answer)"
  let exe = Executable { _main = mainMod, _library = [maliciousMod, secretMod] }
  result <- runHaskellFilesInSandbox (exe, "")
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right (_ec, stdout, stderr) -> do
      let combined = stdout ++ stderr
      assertBool "secret IS leaked via Executable (proving vulnerability)"
        ("the secret is 42" `isInfixOf` combined)

-- Test: simple single-source hello world via runHaskellInSandbox
testSimpleHelloWorld :: Assertion
testSimpleHelloWorld = do
  let source = "module Main where\nmain :: IO ()\nmain = putStrLn \"hello from sandbox\""
  result <- runHaskellInSandbox (source, "")
  case result of
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right (ec, stdout, _stderr) -> do
      ec @?= ExitSuccess
      assertBool "stdout has greeting" ("hello from sandbox" `isInfixOf` stdout)
