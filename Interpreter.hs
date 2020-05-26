{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Control.Monad (forM_, (>=>))
import Control.Monad.Trans

import Either
import Expr
import Eval
import Kernel
import Lexer
import Parser

import Text.Parsec

runProgram :: String -> IO ()
runProgram fp = do
    source <- readFile fp
    runKoala $ case parse (whiteSpace *> (program <* eof)) fp source of
        Left err -> liftIO $ putStr "Error: " >> print err
        Right (lib, mexpr) -> do
            let libExpr = foldr (uncurry ELet) (ECst 0) lib
            case infer [] libExpr of
                Left err -> liftIO $ putStrLn err
                Right (_, libT) ->
                    case mexpr of
                        Nothing -> return ()
                        Just expr ->
                            case infer libT expr of
                                Left err -> liftIO $ putStrLn err
                                Right (t, lib') -> do
                                    liftIO $ forM_ (reverse lib') $ \(x, t) -> do
                                        putStr $ " - " ++ x ++ ": "
                                        print t
                                    r <- whnf [] $ foldr (uncurry ELet) expr lib
                                    liftIO $ putStr "Val:   - " >> putStr (show t) >> putStr ": "
                                    lazyPrint r

lazyPrint :: WHNF -> Koala ()
lazyPrint = list1 >=> (\() -> liftIO $ putStrLn "")
    where printTuples [] = return ()
          printTuples [x] = x
          printTuples (x:xs) = do
              x
              liftIO $ putStr ","
              printTuples xs
          list1 = \case
                WCst k -> liftIO $ putStr $ show k
                WClosure c -> liftIO $ putStr "<fun>" 
                WCpl thks ->  do
                    liftIO $ putStr "("
                    printTuples $ map (\th -> th () >>= list1) thks
                    liftIO $ putStr ")"
                WNil -> liftIO $ putStr "[]"
                WCons th1 th2 -> do
                    liftIO $ putStr "["
                    th1 () >>= list1
                    r <- th2 ()
                    case r of
                        WNil -> liftIO $ putStr "]"
                        _ -> do
                            liftIO $ putStr ","
                            list1' r
          list1' = \case
                WCons th1 th2 -> do
                    th1 () >>= list1
                    r <- th2 ()
                    case r of
                        WNil -> liftIO $ putStr "]"
                        _ -> do
                            liftIO $ putStr ","
                            list1' r
                r  -> list1 r 