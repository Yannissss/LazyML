{-# LANGUAGE LambdaCase #-}
module Kernel where

import           Control.Arrow ((***), second)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans (lift)

import           Data.List

import Debug.Trace

import           Expr

data Type = TVar String
          | TUnit
          | TInt
          | TCpl [Type]
          | TLst Type
          | TFun Type Type
          | TAbs String Type
          deriving (Eq, Ord, Show)

type TEnv       = [(String, Type)]
type Constraint = (Type, Type)
type Subst      = [(String, Type)]
type Solver     = StateT ([Constraint], [String], TEnv) (Either String)

showType :: Type -> String
showType = \case
    TVar x   -> x
    TUnit    -> "()"
    TInt     -> "int"
    TCpl ts  -> "(" ++ intercalate ", " (map showType ts) ++ ")"
    TLst t   -> "[" ++ showType t ++ "]"
    TFun a b -> pars a ++ " -> " ++ recapp b
    TAbs x t -> showType t
    where pars (TFun a b) = "(" ++ showType a ++ " -> " ++ recapp b ++ ")"
          pars (TAbs x t) = "(forall " ++ x ++ ". " ++ showType t ++ ")"
          pars t = showType t
          recapp (TFun u v) = pars u ++ " -> " ++ recapp v
          recapp t          = pars t

tvarsOf :: Type -> [String]
tvarsOf = \case
    TVar x   -> [x]
    TFun a b -> union (tvarsOf a) (tvarsOf b)
    TAbs x t -> delete x (tvarsOf t)
    TCpl ts  -> unions (map tvarsOf ts)
    TLst t   -> tvarsOf t
    _        -> []
    where unions = foldl union []

tsubst :: String -> Type -> Type -> Type
tsubst x u = \case
    TVar y       | x == y -> u
    t@(TAbs y _) | x == y -> t
    TAbs y t              -> TAbs y (tsubst x u t) 
    TFun a b              -> TFun (tsubst x u a) (tsubst x u b)
    TLst t                -> TLst $ tsubst x u t
    TCpl ts               -> TCpl $ map (tsubst x u) ts
    t                     -> t

throw :: String -> Solver a
throw msg = lift (Left $ "Error: " ++ msg)

constraint :: Type -> Type -> Solver ()
constraint t1 t2 = do
    (cs, ns, env) <- get
    put ((t1, t2):cs, ns, env)

fresh :: Solver String
fresh = do
    (cs, ns, env) <- get
    let n = aux ns "" 0
    put (cs, n:ns, env)
    return n
    where alpha       = "abcdefghijklmnopqrstuvwxyz"
          next var n  = var ++ ((alpha !! n) : "") 
          test ns var = var `elem` ns
          aux ns var n
            | (n < length alpha) = let y = next var n in if (test ns y) then aux ns var (n+1) else y 
            | otherwise          = aux ns (var ++ "a") 1

apply :: (Constraint -> Constraint) -> Solver ()
apply f = do
    (cs, ns, env) <- get
    put (map f cs, ns, env)

subst :: Subst -> Type -> Type
subst s t = foldr (uncurry tsubst) t s

push ::  TEnv -> Pattern -> Solver (TEnv, Type)
push env PWdc = do
    z <- fresh
    return (env, TVar z)
push env (PVar x) = do
    z <- fresh
    let t = TVar z
    return ((x, t) : env, t)
push env (PCpl ps)  = do
    (env', ts) <- foldM (\(env', ts) p -> do
        (env'', t) <- push env' p
        return (env'', t : ts)
        ) (env, []) ps
    return (env', TCpl $ reverse ts)
push env PNil = do
    z <- fresh
    return (env, TLst $ TVar z)
push env (PCons p1 p2) = do
    z <- fresh
    (env', t1) <- push env p1
    (env'', t2) <- push env' p2
    constraint (TLst t1) t2
    return (env'', TLst t1)

resolve :: Solver Subst
resolve = do
    (css, ns, env) <- get
    case css of
        []   -> return []
        c:cs -> do 
            put (cs, ns, env)
            case c of
                (TUnit, TUnit)  -> resolve
                (TInt, TInt)    -> resolve
                (TCpl ts, TCpl ts') ->
                    if length ts == length ts'
                        then do
                            zipWithM_ constraint ts ts'
                            resolve
                        else throw "resolve: Different tuple lengths"
                (TLst t1, TLst t2) -> constraint t1 t2 >> resolve
                (TFun a b, TFun a' b') -> do
                    constraint a a'
                    constraint b b'
                    resolve
                (TAbs x a, TAbs y b) -> do
                    z <- fresh
                    constraint (tsubst x (TVar z) a) (tsubst y (TVar z) b)
                    resolve
                (TVar x, t@(TVar y)) | x == y -> do
                    apply $ (tsubst x t *** tsubst x t)
                    s <- resolve
                    return $ (x,t) : map (second $ tsubst x t) s
                (TVar x, t) | x `elem` tvarsOf t ->
                    throw $ "resolve: Cannot construct infinite type " ++ x ++ " ~ " ++ showType t
                (t, TVar x) | x `elem` tvarsOf t ->
                    throw $ "resolve: Cannot construct infinite type " ++ x ++ " ~ " ++ showType t
                (TVar x, t) -> do
                    apply $ (tsubst x t *** tsubst x t)
                    s <- resolve
                    return $ (x,t) : map (second $ tsubst x t) s
                (t, TVar x) -> do
                    apply $ (tsubst x t *** tsubst x t)
                    s <- resolve
                    return $ (x,t) : map (second $ tsubst x t) s
                (a, b) -> throw $ "resolve: Incompatible types " ++ show a ++ " !~ " ++ show b

getType :: TEnv -> String -> Solver Type
getType [] x    = throw $ "getType: Unbound variable " ++ show x 
getType ((x, t):xs) k
    | x == k    = instanciate t
    | otherwise = getType xs k

instanciate :: Type -> Solver Type
instanciate (TAbs x u) = do
    z <- fresh
    instanciate $ tsubst x (TVar z) u
instanciate t = return t

canonize :: Type -> Solver Type
canonize t = do
    (_, ns, _) <- get
    let g = reverse $ zip (tvarsOf t) (map TVar vars)
    return $ subst g t
    where alpha = "abcdefghijklmnopqrstuvwxyz"
          vars = dico $ map (:[]) alpha
          dico l = let l' = concatMap (\x -> map (x:) l) alpha in l ++ dico l'

solve :: TEnv -> Expr -> Solver Type
solve env expr = do
    t <- aux2 env expr
    synth <- resolve
    let t' = subst (reverse synth) t
    canonize t'
    where aux env = \case
                EApp e1 e2 -> do
                    t1 <- aux env e1
                    t2 <- aux env e2
                    a <- fresh
                    b <- fresh
                    constraint t1 $ TFun (TVar a) (TVar b)
                    constraint t2 $ TVar a
                    return $ TVar b
                EFun x e -> do
                    (env', t1) <- push env x
                    t2 <- aux env' e
                    return $ TFun t1 t2
                EVar x -> getType env x
                ELet x e1 e2 -> do
                    (env', t) <- push env x
                    t1 <- aux env e1
                    constraint t t1
                    synth <- resolve
                    env'' <- forM env' $ \(x,t) -> do
                        t' <- canonize $ subst (reverse synth) t
                        return $ (x, foldr TAbs t' (tvarsOf t'))
                    aux env'' e2
                ECst k -> return TInt
                ECpl es -> do
                    ts <- mapM (aux env) es
                    return $ TCpl ts
                EIte e1 e2 e3 -> do
                    t1 <- aux env e1
                    t2 <- aux env e2
                    t3 <- aux env e3
                    constraint t1 TInt
                    constraint t2 t3
                    return t2
                ENil -> TLst . TVar <$> fresh
                ECons e1 e2 -> do
                    t1 <- aux env e1
                    t2 <- aux env e2
                    constraint t2 $ TLst t1
                    return t2
                EFix e -> do
                    z <- fresh
                    t <- aux env e
                    constraint t $ TFun (TVar z) (TVar z)
                    return $ TVar z
                EMatch e cs -> do
                    z <- fresh
                    t <- fresh
                    forM_ cs $ \(x, e') -> do
                        (env', t') <- push env x
                        constraint (TVar z) t'
                        t'' <- aux env' e'
                        constraint (TVar t) t'' 
                    return $ TVar t
                EAex _ e1 e2 -> do
                    t1 <- aux env e1
                    t2 <- aux env e2
                    constraint t1 TInt
                    constraint t2 TInt
                    return TInt
          aux2 env expr = case expr of
                ELet x e1 e2 -> do
                    (env', t) <- push env x
                    t1 <- aux env e1
                    constraint t t1
                    synth <- resolve
                    env'' <- forM env' $ \(x,t) -> do
                        t' <- canonize $ subst (reverse synth) t
                        return $ (x, foldr TAbs t' (tvarsOf t'))
                    aux2 env'' e2
                _ -> do
                    (cs, ns, _) <- get
                    put (cs, ns, env) 
                    aux env expr

infer :: TEnv -> Expr -> Either String (Type, TEnv)
infer env expr = do
    (t, (_, _, env)) <- runStateT (solve env expr) ([], [], [])
    return (t, env)