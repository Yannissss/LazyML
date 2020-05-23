{-# LANGUAGE LambdaCase #-}
module Kernel where

import Data.Functor ((<$))

import Expr

type Loc = Int

data Value = VCst Int
           | VFun String Expr
           deriving (Eq, Show, Ord)

data Thunk = TVal Value
           | TRaw Expr
           deriving (Eq, Show, Ord)

data Post = PRhs EAop Expr
          | PLhs EAop Int
          | PApp Expr
          | PIfe Expr Expr
          | PSave Loc
          | PJump Loc
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
    find :: String -> m Loc
    defer :: Post -> m ()
    next :: m (Maybe (Post, Loc))
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

koalaMachine :: (KoalaMonad m)
                => Expr 
                -> m Value
koalaMachine = \case 
    ECst k -> do
        post <- next
        case post of
            Nothing -> return $ VCst k
            Just (PLhs op k', loc) -> do
                v <- koalaMachine $ ECst (koalaOp op k k')
                jump loc
                return v
            Just (PRhs op e', loc) -> do
                defer (PLhs op k)
                koalaMachine e'
            Just (PIfe e1 e2, loc) -> do
                v <- if k /= 0 
                        then koalaMachine e1 
                        else koalaMachine e2
                jump loc
                return v
            Just (PSave loc, _) -> do
                 
            _ -> raise ValueError
    EVar x -> find x >>= reduceNF
    EApp e1 e2 -> do
        defer $ PApp e2
        koalaMachine e1
    EFun x e -> do
        post <- next
        case post of
            Nothing -> return $ VFun x e
            Just (PApp e', loc) -> do
                push x e' loc
                v <- koalaMachine e
                jump loc
                return v
            _ -> raise ValueError
    ELet x e1 e2 -> do
        loc <- location
        push x e1 (loc + 1)
        koalaMachine e2
    EAex op e1 e2 -> do
        defer $ PRhs op e2
        koalaMachine e1
    EIte e1 e2 e3 -> do
        defer $ PIfe e2 e3
        koalaMachine e1
    where postprocess = \case


pushEnv :: (KoalaMonad m) 
        => [(String, Value)] 
        -> m ()
pushEnv = mapM_ $ \(x,v) -> do
    loc <- push x (ECst 0) 0
    update loc (TVal v)