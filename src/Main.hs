module Main where

import System.Environment (getArgs)
import HProlog.Interpreter

main :: IO ()
main = do
    args <- getArgs
    run args
