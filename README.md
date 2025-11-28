# Ace RunGhcBubbleWrap Library

A library that uses bubblewrap CLI to allow users to submit and run arbitrary haskell source code on a virtual file system. Currently only works with one file.

In the future we will be extending this capacity through nix, to make it easy to run a full project.

### Build & Run

Easiest way is to add to a project through nix.

```nix
-- Override ghc packages
let
  overrides = self: super:
  {
	runGhcBWrap = self.callPackage ./thunks/runGhcBWrap {};
  }
```

Try it out, enter a repl:

```bash
cd runGhcBWrap
nix-shell
nix-shell -p cabal-install
cabal repl
ghc> :l Example.hs
```

### Example.hs Contents

```bash
main :: IO ()
main = do
  testRunGhcBWrap $ unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "module Main where"
    , "import System.FilePath"
    , "import System.Directory"
    , "import Data.Text"
    , "import qualified Data.Text as T"
    , "import Data.ByteString"
    , "import qualified Data.ByteString.Char8 as BS"
    , "import Data.Map"
    , "import qualified Data.Map as Map"
    , "import Data.Vector (Vector)"
    , "import qualified Data.Vector as V"
    , "import Data.Time.Clock (getCurrentTime)"
    , "import System.Directory (listDirectory)"
    , "import System.FilePath ((</>))"
    , "import System.Process (readProcess)"
    , "import Data.Aeson (encode, object, (.=))"
    , "import System.Random (randomRIO)"
    , "import System.Environment"
    , "import Control.Exception (try, SomeException)"
    , "import Control.Monad (forM_)"
    , "import Control.Monad.State (State, evalState, put, get)"
    , ""

    , "main :: IO ()"
    , "main = do"
    -- , "  print =<< getEnv \"PATH\""

    , "  putStrLn \"holy meta mate\"   "
    , "  Prelude.appendFile \"example.txt\" \"im in a file\""
    , "  Prelude.appendFile \"example.txt\" \"im in a file\""
    , "  x <- Prelude.readFile \"example.txt\" "
    , "  putStrLn $ \"haskell readFile contents: \" <> x"
    ]
```
