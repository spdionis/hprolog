module HProlog.Interpreter (run) where

import HProlog.Parser (doParse, parseQuery)
import HProlog.HProlog
import System.Exit (exitSuccess)
import Data.List
import GHC.Types

--for fast testing from ghci
runTest :: IO ()
runTest = run ["test.pro"]

run :: [String] -> IO ()
run [] = error "Supply file name"
run (file:_) = do
        rules <- doParse file
        case rules of
            Left err -> print $ "Parse error: " ++  show err
            Right rules -> loop rules

-- REPL
loop :: Rules -> IO ()
loop rules = do
    putStrLn "Query: "
    query <- getLine
    if query == "quit" then
        exitSuccess
    else
        case parseQuery query of
            Left err -> putStrLn "Query: "
            Right queryTerm -> do
                        (Result result) <- return $ solve rules queryTerm
                        if null result
                        then putStrLn "No solution"
                        else answer result queryTerm
    loop rules

answer :: [Substitution] -> Term -> IO ()
answer []     _       = putStrLn ""
answer (x:xs) t       = do
                            printSolution x t
                            answer xs t

printSolution :: Substitution -> Term -> IO ()
printSolution []            _       = putStrLn "Yes"
printSolution s v@(Var _ _)      = putStr (getValue s v)
printSolution s (Func _ args)  =
            putStrLn $ intercalate "," $ map (getValue s) args

getValue :: Substitution -> Term -> String
getValue s  v@(Var name _) = name ++ "=" ++ printTerm (head (applySubstitutions s [v]))
getValue _ _ = ""


printTerm :: Term -> String
printTerm (Atom value) = value
printTerm (Var name _) = name
printTerm (Func name _) = name

