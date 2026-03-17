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
      , testCase "multiple user modules compiled in isolation" testMultipleUserModules
      , testCase "exit code propagated from runtime error" testExitCodePropagation
      ]
  , testGroup "runHaskellFilesInSandbox"
      [ testCase "trusted executable runs correctly" testTrustedExecutable
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
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right (ec, stdout, _stderr) -> do
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
    Left err -> do
      let errMsg = show err
      assertBool "error mentions compilation failure" ("Phase 1 compilation failed" `isInfixOf` errMsg)
    Right (_ec, stdout, stderr) -> do
      assertBool "secret not leaked in stdout" (not $ "the secret is" `isInfixOf` stdout)
      assertBool "secret not leaked in stderr" (not $ "the secret is" `isInfixOf` stderr)

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
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right (ec, stdout, _stderr) -> do
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
    Left err -> assertFailure $ "Unexpected error: " ++ show err
    Right (ec, _stdout, stderr) -> do
      assertBool "non-zero exit code" (ec /= ExitSuccess)
      assertBool "stderr mentions error" ("kaboom" `isInfixOf` stderr)

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
