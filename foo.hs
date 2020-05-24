--import Eval
import Expr
import Eval
import Parser

foo :: String -> IO ()
foo s = do
    case parseFromString s of
        Left err -> putStr "Error: " >> print err
        Right expr -> do
            putStr "Expr:  "
            print expr
            r <- eval [] expr
            case r of
                Left err -> putStr "Error: " >> print err
                Right v  -> putStr "Val:   " >> print v

bar :: String -> IO ()
bar s = do
    case parseFromString s of
        Left err -> putStr "Error: " >> print err
        Right expr -> do
            putStr "Expr:  "
            print expr
            r <- whnf [] expr
            case r of
                Left err -> putStr "Error: " >> print err
                Right v  -> putStr "Val:   " >> (case v of
                    TCpl thks _ -> putStrLn $ "TCpl " ++ show thks
                    TVal v -> putStrLn $ "TVal " ++ show v
                    TRaw e -> putStrLn $ "TRaw " ++ show e)