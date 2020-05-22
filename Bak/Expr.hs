{-# LANGUAGE LambdaCase #-}
module Expr where

import           Prelude hiding ((<>))

import           Control.Arrow (first)

import           Data.List

import           Text.PrettyPrint

data Motif = MWdc
           | MVar String
           | MCpl [Motif]
           | MNil
           | MLst Motif Motif
           deriving (Eq, Show, Ord)

data EAop = Pow
          | Add | Min | Mul | Div | Mod
          | And | Or 
          | Le | Leq | Ge | Geq | Eq | Df
          deriving (Eq, Show, Ord)

data Expr = ECst Int
          | EVar String
          | EThk Int
          | EApp Expr Expr
          | EFun Motif Expr
          | ELet Motif Expr Expr
          | EAex EAop Expr Expr
          | ECpl [Expr]
          | ENil
          | ELst Expr Expr
          | EIte Expr Expr Expr
          | EMch Expr [(Motif, Expr)]
          deriving (Eq, Show, Ord)

varsOfMotif :: Motif -> [String]
varsOfMotif = \case
    MVar x   -> [x]
    MCpl xs  -> foldl union [] $ map varsOfMotif xs
    MLst x y -> union (varsOfMotif x) (varsOfMotif y)
    _        -> []

varsOfExpr :: Expr -> [String]
varsOfExpr = \case
    EVar x        -> [x]
    EApp e1 e2    -> union (varsOfExpr e1) (varsOfExpr e2)
    EFun m e      -> difference (varsOfExpr e) (varsOfMotif m)
    ELet m e1 e2  -> union (varsOfExpr e1) $ difference (varsOfExpr e2) (varsOfMotif m)
    EAex _ e1 e2  -> union (varsOfExpr e1) (varsOfExpr e2)
    ECpl es       -> unions (map varsOfExpr es)
    ELst e1 e2    -> union (varsOfExpr e1) (varsOfExpr e2)
    EIte e1 e2 e3 -> union (union (varsOfExpr e1) (varsOfExpr e2)) (varsOfExpr e3)
    EMch e cs     -> union (varsOfExpr e) (unions (map (\(m,e) -> 
                        difference (varsOfExpr e) (varsOfMotif m)) cs))
    _             -> []
    where difference u v = filter (`notElem` v) u
          unions = foldl union [] 

substExpr :: String -> Expr -> Expr -> Expr
substExpr x u = \case
    EVar y | x == y   -> u
    EApp e1 e2        -> EApp (substExpr x u e1) (substExpr x u e2)
    EFun y e     | x `notElem` varsOfMotif y -> EFun y (substExpr x u e)
    ELet y e1 e2 | x `notElem` varsOfMotif y -> ELet y (substExpr x u e1) (substExpr x u e2)  
    EAex op e1 e2     -> EAex op (substExpr x u e1) (substExpr x u e2)
    ECpl es           -> ECpl (substExpr x u <$> es)
    ELst e1 e2        -> ELst (substExpr x u e1) (substExpr x u e2)
    EIte e1 e2 e3     -> EIte (substExpr x u e1) (substExpr x u e2) (substExpr x u e3)
    EMch e cs         -> EMch (substExpr x u e) (map (\(m,e) ->
                            if x `elem` varsOfMotif m
                                then (m, e)
                                else (m, substExpr x u e)) cs)
    t                 -> t

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

instance Prettify Motif where
    prettify = render . formatMotif

instance Prettify Expr where
    prettify = render . formatExpr

formatMotif :: Motif -> Doc
formatMotif = \case
    MWdc     -> text "_"
    MVar x   -> text x
    MCpl xs  -> parens (hcat . punctuate comma . map formatMotif $ xs)
    MLst x y -> parens (formatMotif x <> text "::" <> formatMotif y)
    MNil     -> text "[]"

formatExpr :: Expr -> Doc
formatExpr = aux 0
        where aux pad = \case
                ECst k        -> int k
                EVar x        -> text x
                EApp u v      -> recapp pad u <+> pars pad v 
                EFun m e      -> text "fun" <+> formatMotif m <+> text "=>" <+> aux pad e
                ELet m e1 e2  -> let (ds, e) = reclet pad e2 in 
                    if null ds
                        then text "let" <+> formatMotif m <+> text "=" <+> aux pad e1 <+> text "in" $$ e 
                        else text "let" <+> formatMotif m <+> text "=" <+> aux pad e1 $$ vcat ds <+> text "in" $$ e
                EAex op e1 e2 -> pars pad e1 <+> text (prettify op) <+> pars pad e2
                ECpl es       -> parens (hcat . punctuate comma . map formatExpr $ es)
                ELst e1 e2    -> pars pad e1 <> text "::" <> aux pad e2
                ENil          -> text "[]"
                EIte eb e1 e2 -> text "if" <+> aux pad eb $$ nest (pad + 2) (
                       text "then" <+> aux pad e1 
                    $$ text "else" <+>  aux pad e2)
                EMch e cs     -> text "match" <+> aux pad e <+> text "with" $$ vcat (map (\(m,e) -> 
                    nest pad (char '|' <+> formatMotif m <+> text "->" <+> aux (pad + 2) e)) cs)
                EThk nb       -> text "<thunk#" <> int nb <> text ">"
              nl pad = '\n' : (replicate pad ' ')
              pars pad e           = if npars e then parens (aux pad e) else aux pad e
              npars EFun {}    = True
              npars EApp {}    = True
              npars ELet {}    = True
              npars EAex {}    = True
              npars _          = False
              recapp pad (EApp u v) = recapp pad u <+> pars pad v
              recapp pad e          = pars pad e
              reclet pad (ELet m e1 e2) = first (text "and" <+> formatMotif m <+> equals <+> aux pad e1 :) $ reclet pad e2
              reclet pad e              = ([], nest pad (aux (pad + 2) e))