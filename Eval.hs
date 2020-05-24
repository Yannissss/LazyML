{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Eval where

import           Control.Applicative (liftA2)
import           Control.Monad (void, forM, mapM_, (>>))
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List (delete)

import Debug.Trace

import qualified Data.Vector as V

import           Expr
import           Kernel

type RMem = V.Vector (String, Thunk, Loc)
type Red = (StateT (RMem, Int, [Int], [Post]) (Either Error))

debug :: Red ()
debug = do
    (mem, pc, stk, dfs) <- get
    trace ">> Env" $ return ()
    forM [0..pc-1] $ \idx ->
        if idx < V.length mem && 0 <= idx
            then do
                l <- V.indexM mem idx
                trace (">> " ++ show l) $ return ()
            else return () 
    trace (">> PC: " ++ show pc) $ return ()
    trace (">> Stk: " ++ show stk) $ return ()
    trace (">> Dfs: " ++ show dfs) $ return ()

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

instance KoalaMonad Red where
    jump loc = do
        (mem, _, stk, dfs) <- get
        put (mem, loc, stk, dfs)
    save = do
        (mem, pc, stk, dfs) <- get
        put (mem, pc, pc : stk, dfs)
    ret = do
        (mem, _, stk, dfs) <- get
        case stk of
            [] -> raise $ PanicError "Reached bottom of the address stack"
            (a:as) -> put (mem, a, as, dfs)
    location = do
        (_, loc, _, _) <- get
        return loc
    raise err = do 
        debug
        lift . Left $ err
    update loc thk = do
        (mem, pc, stk, dfs) <- get
        (x, _, loc') <- V.indexM mem loc 
        put (mem V.// [(loc, (x, thk, loc'))], pc, stk, dfs)
    thunk loc = do
        (mem, _, _, _) <- get
        (_, thk, loc') <- V.indexM mem loc
        return (thk, loc')
    find x = do
        (mem, pc, _, _) <- get
        let aux loc
                | loc < 0  = raise $ UnboundVar x
                | otherwise = do
                    (y, _, _) <- V.indexM mem loc
                    if x == y
                        then return loc
                        else aux (loc-1)
        let len = V.length mem
        if pc >= len
            then aux (len-1)
            else aux (pc-1)
    defer df = do
        (mem, pc, stk, dfs) <- get
        put (mem, pc, stk, df : dfs)
    next = do
        (mem, pc, stk, dfs) <- get
        case dfs of
            [] -> return Nothing
            (df : dfs') -> do
                put (mem, pc, stk, dfs')
                return $ Just df
    push x e loc = do
        (mem, pc, stk, dfs) <- get
        let len = V.length mem
        if pc >= len
            then do
                put (V.snoc mem (x, TRaw e, loc), len + 1, stk, dfs)
                return len
            else do
                put (mem V.// [(pc, (x, TRaw e, loc))], pc + 1, stk, dfs)
                return pc
    reduceNF loc = debug >> return (VCst 0)

eval :: [(String, Value)] -> Expr -> Either Error Value
eval env expr = let z = runStateT (pushEnv env >> koalaMachine expr <* debug) (V.empty, 0, [], []) in
    trace (show z) (fst <$> z)