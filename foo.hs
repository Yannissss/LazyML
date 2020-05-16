--import Eval
import Expr
import Env
import Kernel
import Parser

import Text.PrettyPrint

import Control.Monad.State

synt :: String -> IO ()
synt s = case parseFromString s of
    Left err -> do
        putStr "Error: "
        print err
    Right e -> putStrLn $ renderStyle (Style PageMode 10 2.0) (formatExpr e)

sbar = do
    s <- readFile "lazy.ml"
    synt s
{-
foo :: String -> IO ()
foo s = do
    case parseFromString s of
        Left err -> print err
        Right expr -> do
            print expr
            putStrLn $ render (formatExpr expr)
            case runStateT (eval expr) bare of
                Left err -> print err
                Right (r, s) -> do
                    print s
                    print r
                    putStrLn $ prettify r

bar :: IO ()
bar = do
    s <- readFile "lazy.ml"
    case parseFromString s of
        Left err -> print err
        Right expr -> do
            print expr
            putStrLn $ render (formatExpr expr)
            case runStateT (eval expr) bare of
                Left err -> print err
                Right (r, s) -> do
                    -- print s
                    print r
                    putStrLn $ prettify r
                    -}