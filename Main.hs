module Main where

import Parser
import Interpreter

import System.IO

removeTrailling :: String -> String
removeTrailling = dropWhile (== ' ') . reverse . dropWhile (== ' ')

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn $ "Welcome to the Koala interpreter v0.1 @ 05/2020"
    repl ""

repl :: String -> IO ()
repl "" = do
    putStr "Koala> "
    hFlush stdout
    next <- getLine
    case next of
        (':':cmd) -> case words cmd of
            ("run":file:argv) -> do
                source <- readFile file
                readEvalPrint source
                repl ""
            (["q"]) -> return ()
            _ -> putStrLn "Error: Unknown command" >> repl ""
        _ -> case parseFromString next of
            Left _ -> repl next
            Right _ -> readEvalPrint next >> repl ""
repl pred = do
    next <- removeTrailling <$> getLine
    case next of
        (';':';':next') -> readEvalPrint (pred ++ reverse next') >> repl ""
        _ -> repl (pred ++ "\n" ++ reverse next)
