module Main ( main ) where

import Menu ( menuInicial )
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Encoding.UTF8 (utf8)

main :: IO()
main = do
    setLocaleEncoding utf8
    menuInicial