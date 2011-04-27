module Main where

import VL.Compiler.CodeGenerator (compile)

import System.FilePath
import System.Environment

usage :: String
usage = "Usage: vl2c <input file> [<output file>]"

main :: IO ()
main = do args <- getArgs
          let (input, output) = case args of
                                  [i]    -> (i, replaceExtension i ".c")
                                  [i, o] -> (i, o)
                                  _      -> error usage
          readFile input >>= writeFile output . compile
