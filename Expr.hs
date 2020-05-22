{-# LANGUAGE LambdaCase #-}
module Expr where

import           Data.List

data EAop = Pow
          | Add | Min | Mul | Div | Mod
          | And | Or 
          | Le | Leq | Ge | Geq | Eq | Df
          deriving (Eq, Show, Ord)

data Expr = ECst Int
          | EVar String
          | EThk Int
          | EApp Expr Expr
          | EFun String Expr
          | EBnd Expr
          | ELet String Expr Expr
          | EAex EAop Expr Expr
          | EIte Expr Expr Expr
          deriving (Eq, Show, Ord)

varsOfExpr :: Expr -> [String]
varsOfExpr = \case
    EVar x        -> [x]
    EApp e1 e2    -> union (varsOfExpr e1) (varsOfExpr e2)
    EFun x e      -> delete x (varsOfExpr e)
    ELet x e1 e2  -> delete x $ union (varsOfExpr e1) (varsOfExpr e2)
    EAex _ e1 e2  -> union (varsOfExpr e1) (varsOfExpr e2)
    EIte e1 e2 e3 -> union (union (varsOfExpr e1) (varsOfExpr e2)) (varsOfExpr e3)
    _             -> []

subst :: String -> Expr -> Expr -> Expr
subst x u = \case
    EVar y | x == y -> u
    EApp e1 e2 -> EApp (subst x u e1) (subst x u e2)
    EFun y e | x /= y -> EFun y (subst x u e)
    ELet y e1 e2 | x /= y -> ELet y (subst x u e1) (subst x u e2)
    EAex op e1 e2 -> EAex op (subst x u e1) (subst x u e2)
    EIte e1 e2 e3 -> EIte (subst x u e1) (subst x u e2) (subst x u e3)
    e@_  -> e

class Prettify a where
    prettify :: a -> String

instance Prettify EAop where
    prettify Pow = "^"
    prettify Add = "+"
    prettify Min = "-"
    prettify Mul = "*"
    prettify Div = "/"
    prettify Mod = "%"
    prettify And = "&&"
    prettify Or  = "||"
    prettify Le  = "<"
    prettify Leq = "<="
    prettify Ge  = ">"
    prettify Geq = ">="
    prettify Eq  = "="
    prettify Df  = "<>"