{-# LANGUAGE LambdaCase #-}
module Kernel where

import           Control.Arrow ((***), second)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans (lift)

import           Data.List

import           Expr

data Type = TVar String
          | TUnit
          | TInt
          | TCpl [Type]
          | TLst Type
          | TFun Type Type
          deriving (Eq, Ord)

data Scheme     = Scheme [String] Type deriving (Eq, Ord)
type TEnv       = [(String, Scheme)]
type Constraint = (Type, Type)
type Subst      = [(String, Type)]
type Solver     = StateT ([Constraint], Int, TEnv) (Either String)

class Unifiable a where
    freeVars :: a -> [String]
    subst :: String -> Type -> a -> a

instance Show Type where
    show = \case
        TVar x   -> x
        TUnit    -> "()"
        TInt     -> "int"
        TCpl ts  -> "(" ++ intercalate ", " (map show ts) ++ ")"
        TLst t   -> "[" ++ show t ++ "]"
        TFun a b -> pars a ++ " -> " ++ recapp b
        where pars (TFun a b) = "(" ++ show a ++ " -> " ++ recapp b ++ ")"
              pars t          = show t
              recapp (TFun u v) = pars u ++ " -> " ++ recapp v
              recapp t          = pars t

instance Unifiable Type where
    freeVars = \case
        TVar x   -> [x]
        TFun a b -> union (freeVars a) (freeVars b)
        TCpl ts  -> unions (map freeVars ts)
        TLst t   -> freeVars t
        _        -> []
    subst x u = \case
        TVar y       | x == y -> u
        TFun a b              -> TFun (subst x u a) (subst x u b)
        TLst t                -> TLst $ subst x u t
        TCpl ts               -> TCpl $ map (subst x u) ts
        t                     -> t

instance Show Scheme where
    show (Scheme [] t)  = show t
    show (Scheme xs t) = "forall " ++ intercalate " " xs ++ ". " ++ show t

instance Unifiable Scheme where
    freeVars (Scheme xs t) = freeVars t `difference` xs
    subst x u s@(Scheme xs t)
        | x `elem` xs = s
        | otherwise   = Scheme xs (subst x u t) 

difference :: (Eq a) => [a] -> [a] -> [a]
difference u v = filter (`notElem` v) u

unions :: (Eq a) => [[a]] -> [a]
unions = foldl union []

throw :: String -> Solver a
throw msg = lift (Left $ "Error: " ++ msg)

constraint :: Type -> Type -> Solver ()
constraint t1 t2 = do
    (cs, ns, env) <- get
    put ((t1, t2):cs, ns, env)

fresh :: Solver String
fresh = do
    (cs, ns, env) <- get
    let z = '?' : show ns
    put (cs, ns + 1, env)
    return z

apply :: (Constraint -> Constraint) -> Solver ()
apply f = do
    (cs, ns, env) <- get
    put (map f cs, ns, env)

substMany :: (Unifiable a) => Subst -> a -> a
substMany s t = foldr (uncurry subst) t (reverse s)

push :: TEnv -> Pattern -> Solver (TEnv, Type)
push env PWdc      = fresh >>= \z -> return (env, TVar z)
push env (PVar x)  = fresh >>= \z -> let t = TVar z in return ((x, Scheme [] t) : env, t)
push env (PCpl ps) = do
    (ts, env') <- foldM (\(ts, env) p -> do
            (env', t) <- push env p
            return (t:ts, env') 
        ) ([], env) ps
    return (env', TCpl $ reverse ts)
push env (PNil)   = fresh >>= \z -> return (env, TLst $ TVar z)
push env (PCons p1 p2) = do
    (env', t1) <- push env p1
    (env'', t2) <- push env' p2
    constraint t2 $ TLst t1
    return (env'', t2) 

match ::  TEnv -> Pattern -> Type -> Solver TEnv
match env PWdc _ = return $ env
match env (PVar x) t = return $ (x, generalize env t) : env
match env (PCpl ps) (TCpl ts) | length ps == length ts = foldM (\env (p, t) -> match env p t) env (zip ps ts)
match env PNil (TLst _ ) = return env
match env (PCons p1 p2) t@(TLst u) = do
    env' <- match env p1 u
    match env' p1 t
match _ p t = throw $ "match: Pattern-match invalid type: " ++ show p ++ " !~ " ++ show t

lookupType :: TEnv -> String -> Solver Type
lookupType [] x    = throw $ "lookupType: Unbound variable " ++ show x 
lookupType ((x, t):xs) k
    | x == k    = instanciate t
    | otherwise = lookupType xs k

instanciate :: Scheme -> Solver Type
instanciate (Scheme [] u) = return u
instanciate (Scheme (x:xs) u) = do
    z <- fresh
    subst x (TVar z) <$> instanciate (Scheme xs u)

generalize :: TEnv -> Type -> Scheme
generalize env t = normalize $ Scheme (tVars `difference` envVars) t
    where envVars = unions (map (freeVars . snd) env)
          tVars = freeVars t

dictionnary :: [String]
dictionnary = aux (map (:[]) alpha)
    where alpha = "abcdefghijklmnopqrstuvwxyz"
          aux l = l ++ aux (concatMap (\x -> map (x:) l) alpha)

normalize :: Scheme -> Scheme
normalize (Scheme xs u) = Scheme (map snd t) $ substMany s u
    where t = zip xs dictionnary
          s = map (second TVar) t

problem :: TEnv -> Expr -> Solver Type
problem env = \case
    EApp e1 e2 -> do
        t1 <- problem env e1
        t2 <- problem env e2
        z <- fresh
        constraint t1 $ TFun t2 (TVar z)
        return $ TVar z
    EFun x e -> do
        (env', t1) <- push env x
        t2 <- problem env' e
        return $ TFun t1 t2
    EVar x -> lookupType env x
    ELet x e1 e2 -> do
        t1 <- problem env e1
        s <- softSolve
        env' <- match env x (substMany s t1)
        problemTop env' e2
    ECst k -> return TInt
    ECpl es -> do
        ts <- mapM (problem env) es
        return $ TCpl ts
    EIte e1 e2 e3 -> do
        t1 <- problem env e1
        t2 <- problem env e2
        t3 <- problem env e3
        constraint t1 TInt
        constraint t2 t3
        return t2
    ENil -> TLst . TVar <$> fresh
    ECons e1 e2 -> do
        t1 <- problem env e1
        t2 <- problem env e2
        constraint t2 $ TLst t1
        return t2
    EFix e -> do
        z <- fresh
        t <- problem env e
        constraint t $ TFun (TVar z) (TVar z)
        return $ TVar z
    EMatch e cs -> do
        z <- fresh
        t <- fresh
        forM_ cs $ \(x, e') -> do
            (env', t') <- push env x
            constraint (TVar z) t'
            t'' <- problem env' e'
            constraint (TVar t) t'' 
        return $ TVar t
    EAex _ e1 e2 -> do
        t1 <- problem env e1
        t2 <- problem env e2
        constraint t1 TInt
        constraint t2 TInt
        return TInt

problemTop :: TEnv -> Expr -> Solver Type
problemTop env = \case
    ELet x e1 e2 -> do
        t1 <- problem env e1
        s <- softSolve
        env' <- match env x (substMany s t1)
        problemTop env' e2
    expr@_ -> do
        (cs, ns, _) <- get
        put (cs, ns, env) 
        problem env expr

softSolve :: Solver Subst
softSolve = do
    (cs, _, _) <- get
    s <- solve
    (_, ns, env) <- get
    put (cs, ns, env)
    return s

solve :: Solver Subst
solve = do
    (css, ns, env) <- get
    case css of
        []   -> return []
        c:cs -> do 
            put (cs, ns, env)
            case c of
                (TUnit, TUnit)  -> solve
                (TInt, TInt)    -> solve
                (TCpl ts, TCpl ts') ->
                    if length ts == length ts'
                        then do
                            zipWithM_ constraint ts ts'
                            solve
                        else throw "solve: Different tuple lengths"
                (TLst t1, TLst t2) -> constraint t1 t2 >> solve
                (TFun a b, TFun a' b') -> do
                    constraint a a'
                    constraint b b'
                    solve
                (TVar x, t@(TVar y)) | x == y -> do
                    apply $ (subst x t *** subst x t)
                    s <- solve
                    return $ (x,t) : map (second $ subst x t) s
                (TVar x, t) | x `elem` freeVars t ->
                    throw $ "solve: Cannot construct infinite type " ++ x ++ " ~ " ++ show t
                (t, TVar x) | x `elem` freeVars t ->
                    throw $ "solve: Cannot construct infinite type " ++ x ++ " ~ " ++ show t
                (TVar x, t) -> do
                    apply $ (subst x t *** subst x t)
                    s <- solve
                    return $ (x,t) : map (second $ subst x t) s
                (t, TVar x) -> do
                    apply $ (subst x t *** subst x t)
                    s <- solve
                    return $ (x,t) : map (second $ subst x t) s
                (a, b) -> throw $ "solve: Incompatible types " ++ show a ++ " !~ " ++ show b

infer :: TEnv -> Expr -> Either String (Type, TEnv)
infer env expr = do
    (t, (_, _, env)) <- runStateT (do
                            t <- problemTop env expr
                            (pb, _, env) <- get
                            s <- solve
                            return $ substMany s t
                        ) ([], 0, [])
    return (t, env)