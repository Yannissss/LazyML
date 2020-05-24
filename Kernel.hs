{-# LANGUAGE LambdaCase #-}
module Kernel where

import Data.Functor ((<$))

import Debug.Trace

import Expr

type Loc = Int

data Value = VCst Int
           | VFun String Expr
           deriving (Eq, Show, Ord)

data Thunk = TVal Value
           | TRaw Expr
           deriving (Eq, Show, Ord)

data Post = PRhs Loc EAop Expr
          | PLhs Loc EAop Int
          | PIfe Loc Expr Expr
          | PApp Expr Loc
          | PSave Loc Loc
          deriving (Eq, Show, Ord)

data Error = ValueError
           | UnboundVar String
           | PanicError String
           deriving (Eq, Show, Ord)

class Monad m => KoalaMonad m where
    jump :: Loc -> m ()
    save :: m ()
    ret :: m ()
    location :: m Loc
    raise :: Error -> m a
    update :: Loc -> Thunk -> m ()
    thunk :: Loc -> m (Thunk, Loc)
    find :: String -> m Loc
    defer :: Post -> m ()
    next :: m (Maybe Post)
    push :: String -> Expr -> Loc -> m Loc
    reduceNF :: Loc -> m Value

koalaOp :: EAop -> (Int -> Int -> Int)
koalaOp Add = (+)
koalaOp Min = (-)
koalaOp Mul = (*)
koalaOp Div = div
koalaOp Mod = mod
koalaOp And = \x y -> if (x /= 0) && (y /= 0) then y else 0 
koalaOp Or  = \x y -> if x /= 0 then x else y
koalaOp Le  = \x y -> if x < y then 1 else 0
koalaOp Leq = \x y -> if x <= y then 1 else 0
koalaOp Ge  = \x y -> if x > y then 1 else 0
koalaOp Geq = \x y -> if x >= y then 1 else 0
koalaOp Eq  = \x y -> if x == y then 1 else 0
koalaOp Df  = \x y -> if x /= y then 1 else 0

postprocess :: (KoalaMonad m)
            => Value
            -> m Value
postprocess v = do
    post <- next
    case post of
        Nothing -> return v
        Just p -> case (v,p) of
            (_, PSave loc loc') -> do
                update loc (TVal v)
                jump loc'
                postprocess v
            (VCst k', PLhs loc op k) -> do
                jump loc
                postprocess $ VCst (koalaOp op k k')
            (VCst k, PRhs loc op e) -> do
                defer $ PLhs loc op k
                koalaMachine e
            (VCst k, PIfe loc e1 e2) -> do
                jump loc 
                if k /= 0 
                    then koalaMachine e1 
                    else koalaMachine e2
            (VFun x e, PApp e' loc) -> do
                push x e' loc
                koalaMachine e
            _ -> raise ValueError

koalaMachine :: (KoalaMonad m)
             => Expr 
             -> m Value
koalaMachine = \case 
    ECst k -> postprocess $ VCst k
    EFun x e -> postprocess $ VFun x e
    EVar x -> do
        loc <- find x
        (thk, loc') <- thunk loc
        case thk of
            TVal v -> postprocess v
            TRaw e -> do
                loc'' <- location
                defer $ PSave loc loc''
                jump loc'
                koalaMachine e
    EApp e1 e2 -> do
        loc <- location
        defer $ PApp e2 loc
        koalaMachine e1
    ELet x e1 e2 -> do
        loc <- location
        push x e1 (loc + 1)
        koalaMachine e2
    EAex op e1 e2 -> do
        loc <- location
        defer $ PRhs loc op e2
        koalaMachine e1
    EIte e1 e2 e3 -> do
        loc <- location
        defer $ PIfe loc e2 e3
        koalaMachine e1

pushEnv :: (KoalaMonad m) 
        => [(String, Value)] 
        -> m ()
pushEnv = mapM_ $ \(x,v) -> do
    loc <- push x (ECst 0) 0
    update loc (TVal v)