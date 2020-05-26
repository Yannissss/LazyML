module Main where

import Control.Monad

import Parser
import Kernel

import System.IO

options :: [String, [String], [String] -> IO ()]
options = []

removeTrailling :: String -> String
removeTrailling = dropWhile (== ' ') . reverse . dropWhile (== ' ')

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn $ "Welcome to the Koala interpreter v0.1 @ 05/2020"
    repl

repl :: IO ()
repl = do
    putStr "Koala> "
    hFlush stdout
    next <- getLine
    case parseFromString next of
        Left err -> putStr "Error: " >> print err >> repl
        Right expr -> do
            putStrLn $ "Expr:  " ++ show expr
            case infer [] expr of
                Left err -> putStrLn err >> repl
                Right (t, lib) -> do
                    forM_ (reverse lib) $ \(x, s) -> do
                        putStr $ " - " ++ x ++ ": "
                        print s
                    putStr "Type:  "
                    print t
                    repl