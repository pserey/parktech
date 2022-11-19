module Lib
    ( someFunc
    ) where

import System.IO.Strict (readFile)
import Prelude hiding (readFile)

someFunc :: IO ()
someFunc = do
    file <- readFile "README.md"
    putStrLn file
