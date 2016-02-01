module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doctest'
  where doctest' files = doctest $ "-Iinclude/" : "-D__DOCTEST__" : files
