{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RunGhcBWrap where

-- import Network.Wai (Application, responseLBS, getRequestBodyChunk)
-- import Network.HTTP.Types (status200, status500)
-- import Network.Wai.Handler.Warp (run)
-- import Data.Text (Text, unpack)
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as TE
-- import qualified Data.ByteString.Lazy as LBS
-- import GHC (Ghc, runGhc, guessTarget, setTargets, targetContents, getSessionDynFlags, setSessionDynFlags, load, LoadHowMuch(..), setContext, compileExpr, simpleImportDecl, mkModuleName, InteractiveImport(IIDecl), Target, HValue)
-- import GHC.Paths (libdir)
-- import GHC.Driver.Session (DynFlags(..), GhcLink(..), parseDynamicFlagsCmdLine)
-- import GHC.Driver.Backend (interpreterBackend)
-- import GHC.Data.StringBuffer (stringToStringBuffer)
-- import GHC.Utils.Panic (GhcException (..), throwGhcExceptionIO)
-- import GHC.Types.Basic
-- import GHC.Types.SrcLoc (noLoc)
-- import Data.Time (getCurrentTime)
-- import Control.Monad (when)
-- import Control.Monad.IO.Class (liftIO)
-- import Control.Exception (try, SomeException, throwIO)
-- import System.IO.Unsafe (unsafePerformIO)
-- import System.IO (hPutStrLn, stdout, stderr)
-- import System.IO.Error (userError)
-- import System.IO.Silently (hCapture)
-- import Unsafe.Coerce (unsafeCoerce)
-- import Data.IORef (newIORef, readIORef, writeIORef)

import System.Which
import System.Process as P --(readCreateProcessWithExitCode, proc)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>), takeDirectory)
import System.Exit (ExitCode)
import Control.Exception (try, SomeException, displayException)
import System.Environment (getEnv)
import System.Directory (createDirectoryIfMissing)

import Control.Monad

runghc = $(staticWhich "runghc")
ghc = $(staticWhich "ghc")
runghc912 = $(staticWhich "runghc-9.12.2")
ghc912 = $(staticWhich "ghc-9.12.2")
bubblewrap = $(staticWhich "bwrap")


nix = $(staticWhich "nix-shell")

-- nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ temporary vector aeson ])" bubblewrap cabal-install which --run "runghc TestBWrap.hs" --pure

testRunGhcBWrap :: String -> IO ()
testRunGhcBWrap script = do
  putStrLn $ "With runghc: " <> runghc912
  --print (runghc912, runghc, ghc, bubblewrap)
  x <- runHaskellInSandbox script

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

-- | Run Haskell source code in a sandboxed environment
runHaskellInSandbox :: String -> IO (Either SomeException (ExitCode, String, String))
runHaskellInSandbox sourceCode = try $ do
  withSystemTempDirectory "sandbox" $ \tmpDir -> do
    let projectDir = tmpDir </> "project"
    let tmpBindDir = tmpDir </> "tmp"

    createDirectoryIfMissing True projectDir
    createDirectoryIfMissing True tmpBindDir
    createDirectoryIfMissing True $ projectDir </> "src"
    let hsFile = projectDir </> "src" </> "Main.hs"
    writeFile hsFile sourceCode
    hostPath <- getEnv "PATH"
    let bwrapCmd = P.proc bubblewrap
          [ "--bind", projectDir, "/project"
          , "--bind", tmpBindDir, "/tmp"
          , "--dev", "/dev"
          , "--proc", "/proc"
          , "--ro-bind", "/nix/store", "/nix/store"
          , "--setenv", "PATH", hostPath --takeDirectory runghc --hostPath
          , "--setenv", "TMPDIR", "/tmp"
          , "--chdir", "/project"
          -- , "cabal", "build", "--offline"
          , runghc912, "-f", ghc912, "src/Main.hs"
          ]
    readCreateProcessWithExitCode bwrapCmd ""



-- --------------------------------------------------------------------------------
-- -- Server entrypoint
-- --------------------------------------------------------------------------------

-- main :: IO ()
-- main = do
--   putStrLn "Starting server on port 8080..."
--   run 8080 app


-- --------------------------------------------------------------------------------
-- -- WAI Application: accepts code via POST body and returns output or error
-- --------------------------------------------------------------------------------

-- app :: Application
-- app req respond = do
--   -- Read POST body (single chunk)
--   body <- getRequestBodyChunk req
--   let code = TE.decodeUtf8 body

--   -- Run the Haskell code through GHC API
--   result <- runHaskellCode code

--   -- Respond with either output or error text
--   respond $ case result of
--     Right output -> responseLBS status200 [("Content-Type", "text/plain")]
--                       (LBS.fromStrict $ TE.encodeUtf8 output)
--     Left err     -> responseLBS status500 [("Content-Type", "text/plain")]
--                       (LBS.fromStrict $ TE.encodeUtf8 err)


-- --------------------------------------------------------------------------------
-- -- Wrap user code in a synthetic module that defines `main`
-- -- This lets us compile it as a real file with GHC API
-- --------------------------------------------------------------------------------

-- runDynamicCodeTemplate :: Text -> Text
-- runDynamicCodeTemplate codeText =
--   T.unlines
--     [ "module DynamicCode where"
--     , "import System.IO (hFlush, stdout)"
--     , "import Data.Time.Clock (UTCTime(..), getCurrentTime)"
--     , "main :: IO ()"
--     , "main = do"
--     , "  " <> codeText            -- user code injected here
--     , "  hFlush stdout"          -- ensure buffered output is flushed
--     , "  pure ()"
--     ]


-- --------------------------------------------------------------------------------
-- -- Configure a fresh GHC session for *each* request
-- -- This installs package flags, uses in-memory linking, and provides source text
-- --------------------------------------------------------------------------------

-- setupGhcSession :: String -> Ghc Target
-- setupGhcSession codeStr = do
--   -- Package arguments to enable inside dynamic interpreter GHC
--   -- These must exist in the GHC package DB used at runtime
--   let pkgArgs =
--         [ "-package", "time" ]
--         -- Add more packages here if they exist in your environment

--   -- Get the initial DynFlags from the session
--   dflags <- getSessionDynFlags

--   -- Apply package flags like: -package time, etc.
--   (pflags, _, _) <- parseDynamicFlagsCmdLine dflags (map noLoc pkgArgs)

--   -- Reconfigure DynFlags to use the bytecode interpreter
--   let dflags' =
--         pflags { ghcLink = LinkInMemory
--                , backend = interpreterBackend
--                }

--   -- Install updated DynFlags into session
--   _ <- setSessionDynFlags dflags'

--   -- Create a synthetic "DynamicCode.hs" target containing our provided source
--   target <- guessTarget "DynamicCode.hs" Nothing Nothing
--   now <- liftIO getCurrentTime
--   let target' =
--         target { targetContents = Just (stringToStringBuffer codeStr, now) }

--   pure target'


-- --------------------------------------------------------------------------------
-- -- Load the in-memory module and fail out on error
-- --------------------------------------------------------------------------------

-- loadModule :: Target -> Ghc ()
-- loadModule target = do
--   setTargets [target]
--   result <- load LoadAllTargets
--   when (isFailed result) $
--     liftIO $ throwIO (userError "Failed to load DynamicCode module!")


-- --------------------------------------------------------------------------------
-- -- Compile the expression "DynamicCode.main :: IO ()" and return as HValue
-- --------------------------------------------------------------------------------

-- compileMain :: Ghc HValue
-- compileMain = do
--   -- Make Prelude and the synthetic module available for evaluation
--   setContext
--     [ IIDecl $ simpleImportDecl (mkModuleName "Prelude")
--     , IIDecl $ simpleImportDecl (mkModuleName "DynamicCode")
--     ]

--   -- Compile the main function into an HValue (runtime value)
--   compileExpr "DynamicCode.main :: IO ()"


-- --------------------------------------------------------------------------------
-- -- Execute the runtime IO value, capturing stdout
-- --------------------------------------------------------------------------------

-- captureOutput :: HValue -> IO String
-- captureOutput hval = do
--   -- Unsafe coercion because GHC API returns HValue instead of typed value
--   let ioAction = unsafeCoerce hval :: IO ()

--   -- Capture stdout while running the user code
--   (output, ()) <- hCapture [stdout] ioAction
--   pure output


-- --------------------------------------------------------------------------------
-- -- Wrap result into Text-based error/success responses
-- --------------------------------------------------------------------------------

-- handleResult :: Either SomeException String -> IO (Either Text Text)
-- handleResult result =
--   case result of
--     Left err -> do
--       hPutStrLn stderr $ "Error: " ++ show err
--       pure $ Left $ "Error: " <> T.pack (show err)

--     Right output ->
--       pure $ Right $ T.pack output


-- --------------------------------------------------------------------------------
-- -- Top-level: wrap user code, run it through GHC API, execute it
-- --------------------------------------------------------------------------------

-- runHaskellCode :: Text -> IO (Either Text Text)
-- runHaskellCode codeText = do
--   -- Build synthetic module source text
--   let wrappedCode = runDynamicCodeTemplate codeText
--       codeStr     = unpack wrappedCode

--   -- Safely run the whole GHC process with error capture
--   result <- try $ runGhc (Just libdir) $ do
--     target <- setupGhcSession codeStr
--     _      <- loadModule target
--     hval   <- compileMain
--     liftIO $ captureOutput hval

--   handleResult result


-- --------------------------------------------------------------------------------
-- -- Helper to unwrap GHC SuccessFlag
-- --------------------------------------------------------------------------------

-- isFailed :: SuccessFlag -> Bool
-- isFailed Failed = True
-- isFailed _      = False
