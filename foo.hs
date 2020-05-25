
import Expr
import Eval
import Parser

import Either

foo :: String -> IO ()
foo s = 
    case parseFromString s of
        Left err -> putStr "Error: " >> print err
        Right expr -> do
            putStr "Expr:  "
            print expr
            r <- runEitherT $ whnf [] expr
            case r of
                Left err -> putStr "Error: " >> print err
                Right v  -> putStr "Val:   " >> (unwrap $ lazyPrint v)
    where unwrap = (\m -> do
            r <- m
            case r of
                Left err -> putStr "Error: " >> print err
                Right _ -> return ()) . runEitherT 

bar :: String -> IO ()
bar s = 
    case parseFromString s of
        Left err -> putStr "Error: " >> print err
        Right expr -> do
            putStr "Expr:  "
            print expr
            r <- runEitherT $ whnf [] expr
            case r of
                Left err -> putStr "Error: " >> print err
                Right v  -> putStr "Val:   " >> print v

load :: String -> IO ()
load f = do
    s <- readFile f
    case parseFromString s of
        Left err -> putStr "Error: " >> print err
        Right expr -> do
            putStr "Expr:  "
            print expr
            r <- runEitherT $ whnf [] expr
            case r of
                Left err -> putStr "Error: " >> print err
                Right v  -> putStr "Val:   " >> (unwrap $ lazyPrint v)
    where unwrap = (\m -> do
            r <- m
            case r of
                Left err -> putStr "Error: " >> print err
                Right _ -> return ()) . runEitherT 