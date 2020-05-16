{-# LANGUAGE LambdaCase #-}
module Eval where

import           Control.Applicative (liftA2)
import           Control.Monad (void)
import           Control.Monad.State

import Debug.Trace

import qualified Data.Vector as V

import           Env
import           Expr

data NF = NFCst Int
        | NFNil
        | NFFun Motif Expr
        | NFCpl NF NF
        | NFLst NF NF
        deriving (Eq, Show, Ord)

instance Prettify NF where
    prettify = \case
        NFCst k -> show k
        NFNil -> "[]"
        NFFun _ _ -> "<fun>"
        NFCpl x y -> "(" ++ prettify x ++ ", " ++ prettify y ++ ")"
        NFLst x y -> "[" ++ prettify x ++ reclst y ++ "]"
        where reclst NFNil = ""
              reclst (NFLst x y) = ";" ++ prettify x ++ reclst y
              reclst nf = prettify nf

data Thunk = TNF NF
           | TCpl Thunk Thunk
           | TLst Thunk Thunk
           | TRaw Expr
           | TRef Int
           deriving (Eq, Show, Ord)

data Decl = DRaw [String] Motif Int Int
          | DBnd String Int Int
          deriving (Eq, Show, Ord)

data RError = ValueError
            | UnboundVar String
            | NoMatch
            | PanicError
            | ConflictDef String
            deriving (Eq, Show, Ord)

type REnv = V.Vector [Decl]
type RState = (REnv, V.Vector Thunk)
type Red = StateT RState (Either RError)

bare :: (REnv, V.Vector Thunk)
bare = ([], V.empty)

evalOp :: EAop -> (Int -> Int -> Int)
evalOp Add = (+)
evalOp Min = (-)
evalOp Mul = (*)
evalOp Div = div
evalOp Mod = mod
evalOp And = \x y -> if (x /= 0) && (y /= 0) then y else 0 
evalOp Or  = \x y -> if x /= 0 then x else y
evalOp Le  = \x y -> if x < y then 1 else 0
evalOp Leq = \x y -> if x <= y then 1 else 0
evalOp Ge  = \x y -> if x > y then 1 else 0
evalOp Geq = \x y -> if x >= y then 1 else 0
evalOp Eq  = \x y -> if x == y then 1 else 0
evalOp Df  = \x y -> if x /= y then 1 else 0

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

err :: RError -> Red a
err = lift . Left

getEnv :: Red REnv
getEnv = do
    (env, _) <- get
    return env

putEnv :: REnv -> Red ()
putEnv env = do
    (_, mtable) <- get
    put (env, mtable)

push :: Motif -> Expr -> Red ()
push m e = do
    env <- getEnv    
    let vars    = varsOfMotif m
        free    = filter (`notElem` vars) (varsOfExpr e)
        last    = V.length env
    putEnv $ V.snoc (DRaw free m e last) env 

local :: Red a -> Red a
local s = do
    env <- getEnv
    r <- s
    putEnv env
    return r

restrict :: Int -> Red a -> Red a
restrict limit s = do
    env <- getEnv
    putEnv $ V.slice 0 limit env
    r <- s
    putEnv env
    return r

unify :: Motif -> Int -> Red ()
unify 

find :: String -> Red Int
find x = do
    env <- getEnv
    let aux = (\case 
            DBnd y _ _    -> x == y
            DRaw xs _ _ _ -> x `elem` xs)
        (l,r) = break aux env
    case r of
        [] -> err $ UnboundVar x
        (d:ds) -> case d of
            DBnd _ nb _ -> return nb
            DRaw xs m nb last -> do
                restrict last $ do



update :: Int -> Thunk -> Red ()
update nb thk = do
    (env, mtable) <- get
    put (env, mtable V.// [(nb, thk)])

{-
reduceWHNF :: Int -> Red Thunk
reduceWHNF nb = do
    thk <- repr nb
    case thk of
        TRef _' -> err PanicError 
        TRaw xs e ->  
            let aux (x, k) = substExpr x (EThk k)
            in
            case foldr aux e xs of
            ECst k -> update nb $ TNF $ NFCst k
            EFun x e -> update nb $ TNF $ NFFun x e
            ENil -> update nb $ TNF $ NFNil
            EVar x -> do
                nb' <- find x
                update nb (TRef nb')
            EThk nb' -> do
                update nb $ TRef nb'
                reduceWHNF nb'
            ECpl x y -> do
                nb1 <- save x
                nb2 <- save y
                update nb $ TCpl (TRef nb1) (TRef nb2)
            ELst x y -> do
                nb1 <- save x
                nb2 <- save y
                update nb $ TLst (TRef nb1) (TRef nb2)
            EAex op e1 e2 -> do
                lhs <- eval e1
                rhs <- eval e2
                case (lhs, rhs) of
                    (NFCst k1, NFCst k2) -> update nb $ TNF $ NFCst $ evalOp op k1 k2
                    _ -> err ValueError
            EApp e1 e2 -> do
                lhs <- eval e1
                case lhs of
                    NFFun m e -> do
                        nb <- save e2
                        nb' <- save e
                        e' <- unify nb m e 
                        update nb' $ TRaw [] e'
                        reduceWHNF nb'
                    _ -> err ValueError
            ELet m e1 e2 -> do
                nb <- save e1
                nb' <- save e2
                e2' <- unify nb m e2
                update nb $ TRaw [] e2' 
                reduceWHNF nb'
            EIte eb e1 e2 -> do
                rb <- eval eb
                case rb of
                    NFCst t ->
                        if t /= 0
                            then do
                                nb' <- save e1
                                reduceWHNF nb'
                                update nb $ TRef nb'
                            else do
                                nb' <- save e2
                                reduceWHNF nb'
                                update nb $ TRef nb'
                    _ -> err ValueError
            EMch e cs -> do
                nb' <- save e
                e' <- unifyMatch nb' cs
                nb'' <- save e'
                update nb $ TRef nb''
                reduceWHNF nb''
        _ -> return ()

reduceNF :: Int -> Red NF
reduceNF nb = do
    thk <- repr nb
    case thk of
        TRef _' -> err PanicError 
        TRaw xs e ->  
            let aux (x, k) = substExpr x (EThk k)
            in
            case foldr aux e xs of
            ECst k -> do
                let nf = NFCst k
                update nb (TNF nf)
                return nf
            EFun x e -> do
                let nf = NFFun x e
                update nb (TNF nf)
                return nf
            ENil -> do
                let nf = NFNil
                update nb (TNF nf)
                return nf
            EVar x -> do
                nb' <- find x
                reduceNF nb'
            EThk nb' -> do
                nf <- reduceNF nb'
                update nb $ TRef nb'
                return nf
            ECpl x y -> do
                nf1 <- eval x
                nf2 <- eval y
                let nf = NFCpl nf1 nf2
                update nb (TNF nf)
                return nf
            ELst x y -> do
                nf1 <- eval x
                nf2 <- eval y
                let nf = NFLst nf1 nf2
                update nb (TNF nf)
                return nf
            EAex op e1 e2 -> do
                lhs <- eval e1
                rhs <- eval e2
                case (lhs, rhs) of
                    (NFCst k1, NFCst k2) -> do
                        let nf = NFCst $ evalOp op k1 k2
                        update nb (TNF nf)
                        return nf
                    _ -> err ValueError
            EApp e1 e2 -> do
                lhs <- eval e1
                case lhs of
                    NFFun m e -> do
                        nb2 <- save e2
                        e' <- unify nb2 m e
                        update nb $ TRaw [] e'
                        reduceNF nb
                    _ -> err ValueError
            ELet m e1 e2 -> do
                nb1 <- save e1
                nb2 <- save e2
                e2' <- unify nb1 m e2
                update nb $ TRaw [] e2'
                reduceNF nb
            EIte eb e1 e2 -> do
                rb <- eval eb
                case rb of
                    NFCst t ->
                        if t /= 0
                            then eval e1
                            else eval e2
                    _ -> err ValueError
            EMch e cs -> do
                nb' <- save e
                e' <- unifyMatch nb' cs
                nb'' <- save e'
                update nb $ TRef nb''
                reduceNF nb''
        TCpl thk1 thk2 -> do
            nb1 <- new thk1
            nb2 <- new thk2
            nf1 <- reduceNF nb1
            nf2 <- reduceNF nb2
            return $ NFCpl nf1 nf2
        TLst thk1 thk2 -> do
            nb1 <- new thk1
            nb2 <- new thk2
            nf1 <- reduceNF nb1
            nf2 <- reduceNF nb2
            return $ NFLst nf1 nf2
        TNF nf -> return nf
-}

eval :: Expr -> Red NF
eval e = local $ case e of
    ECst k -> return $ NFCst k
    EVar x -> findNF x
    EFun x e -> return $ NFFun x e
    ELet m e1 e2 -> do
        push m e1
        eval e2
    EAex op e1 e2 -> do
        lhs <- eval e1
        rhs <- eval e2
        case (lhs, rhs) of
            (NFCst k1, NFCst k2) -> return $ NFCst $ evalOp op k1 k2
            _ -> err ValueError
    EApp e1 e2 -> do
        v1 <- eval e1
        case v1 of 
            NFFun m e -> do
                push m e2
                eval e
            _ -> err ValueError
    ECpl e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        return $ NFCpl v1 v2
    ELst e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        return $ NFLst v1 v2
    ENil -> return NFNil
    EIte eb e1 e2 -> do
        v <- eval eb
        case v of
            NFCst t ->
                if t /= 0
                    then eval e1
                    else eval e2
            _ -> err ValueError
    EMch e cs -> do
        e' <- unifyMatch e cs
        eval e'