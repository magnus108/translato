module Main where

import qualified Lib

import           Options.Generic

data Config = Config { port :: Int }
    deriving Generic
    deriving anyclass ParseRecord


main :: IO ()
main = do
    Config (port) <- getRecord "Run"
    Lib.main port ""
