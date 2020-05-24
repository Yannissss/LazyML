{-# LANGUAGE LambdaCase #-}
module Expr where

import           Data.List

data Pattern = PWdc
             | PVar String
             | PCpl [Pattern]
             deriving (Eq, Show, Ord)

data EAop = Pow
          | Add | Min | Mul | Div | Mod
          | And | Or 
          | Le | Leq | Ge | Geq | Eq | Df
          deriving (Eq, Show, Ord)

data Expr = ECst Int
          | EVar String
          | EApp Expr Expr
          | EFun Pattern Expr
          | ECpl [Expr]
          | ELet [(Pattern, Expr)] Expr
          | EAex EAop Expr Expr
          | EIte Expr Expr Expr
          deriving (Eq, Show, Ord)

varsOfPattern :: Pattern -> [String]
varsOfPattern = \case
    PVar x   -> [x]
    PCpl xs  -> foldl union [] $ map varsOfPattern xs
    _        -> []

varsOfExpr :: Expr -> [String]
varsOfExpr = \case
    EVar x        -> [x]
    EApp e1 e2    -> union (varsOfExpr e1) (varsOfExpr e2)
    EFun x e      -> difference (varsOfExpr e) (varsOfPattern x)
    ECpl es       -> unions $ map varsOfExpr es
    ELet ds e2    -> let vars = unions $ map (varsOfPattern . fst) ds
                         evars = map ((`difference` vars) . varsOfExpr . snd) ds in
        union (varsOfExpr e2 `difference` vars) (unions evars)
    EAex _ e1 e2  -> union (varsOfExpr e1) (varsOfExpr e2)
    EIte e1 e2 e3 -> union (union (varsOfExpr e1) (varsOfExpr e2)) (varsOfExpr e3)
    _             -> []
    where difference u v = filter (`notElem` v) u
          unions = foldl union [] 

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