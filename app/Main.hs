module Main where

import qualified Lib

import           Options.Generic

data Config = Config { port :: Int, root :: FilePath }
    deriving Generic
    deriving anyclass ParseRecord


main :: IO ()
main = do
    Config port root <- getRecord "Run"
    Lib.main port root
