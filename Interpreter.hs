{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Control.Monad (forM_, (>=>))
import Control.Monad.Trans

import Either
import Eval
import Expr
import Kernel
import Parser

readEvalPrint :: String -> IO ()
readEvalPrint s = runKoala $
    case parseFromString s of
        Left err -> liftIO $ putStr "Error: " >> print err
        Right expr -> do
            -- liftIO $ putStr "Expr:  "
            -- liftIO $ print expr
            case infer [] expr of
                Left err -> liftIO $ putStr "Error: " >> putStrLn err
                Right (t, lib) -> do
                    -- liftIO $ print lib
                    liftIO $ forM_ (reverse lib) $ \(x, t) -> do
                        putStr $ " - " ++ x ++ ": "
                        putStrLn $ showType t
                    r <- whnf [] expr
                    liftIO $ putStr "Val:   - " >> putStr (showType t) >> putStr ": "
                    lazyPrint r

lazyPrint :: WHNF -> Koala ()
lazyPrint = aux >=> (\() -> liftIO $ putStrLn "")
    where pint [] = return ()
          pint [x] = x
          pint (x:xs) = do
              x
              liftIO $ putStr ","
              pint xs
          aux = \case
                WCst k -> liftIO $ putStr $ show k
                WClosure c -> liftIO $ putStr "<fun>" 
                WCpl thks ->  do
                    liftIO $ putStr "("
                    pint $ map (\th -> th () >>= aux) thks
                    liftIO $ putStr ")"
                WNil -> liftIO $ putStr "[]"
                WCons th1 th2 -> do
                    liftIO $ putStr "["
                    th1 () >>= aux
                    r <- th2 ()
                    case r of
                        WNil -> liftIO $ putStr "]"
                        _ -> do
                            liftIO $ putStr ","
                            aux' r
          aux' = \case
                WCons th1 th2 -> do
                    th1 () >>= aux
                    r <- th2 ()
                    case r of
                        WNil -> liftIO $ putStr "]"
                        _ -> do
                            liftIO $ putStr ","
                            aux' r
                r  -> aux r 