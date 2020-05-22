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
            case eval [] expr of
                Left err -> putStr "Error: " >> print err
                Right v  -> putStr "Val:   " >> print v