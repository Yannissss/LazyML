{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Eval where

import           Control.Applicative (liftA2)
import           Control.Monad (void, forM, mapM_, zipWithM, (>>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans

import           Data.List (delete)

import           Data.IORef

import           Either
import           Expr

data Value = VCst Int
           | VFun Pattern Expr
           | VCpl [Value]
           deriving (Eq, Show, Ord)

data Thunk = TVal Value
           | TRaw Expr
           | TCpl [Expr] Env

data Cont = CLhs EAop Int
          | CRhs EAop Expr Env
          | CCpl [Expr] [Value] Env
          | CApp Expr Env
          | CIte Expr Expr Env
          | CSave (IORef Thunk)

data Error = ValueError
           | MatchError
           | UnboundVar String
           | PanicError String
           deriving (Eq, Show, Ord)

data Decl = DRaw [String] Pattern Expr
          | DBnd [(String, (IORef Thunk))]
data Env = Env [(IORef Decl, Env)]
type Koala = ReaderT Env (StateT [Cont] (EitherT Error IO))

data State = State Stack Env [(Stack, Env)]

debug env = do
    liftIO $ putStrLn "[Debug]"
    forM_ env $ \(ref, _) -> do
        d <- liftIO $ readIORef ref
        aux d
    liftIO $ putStrLn "[/Debug]"
    where aux = \case
            DRaw vars x expr -> liftIO $ print (vars, x, expr)
            DBnd ds -> forM_ ds $ \(x,ref) -> do
                thk <- liftIO $ readIORef ref
                liftIO $ case thk of 
                    TCpl thks _ -> putStrLn $ "TCpl " ++ show thks
                    TVal v -> putStrLn $ "TVal " ++ show v
                    TRaw e -> putStrLn $ "TRaw " ++ show e

    {-
debug :: Koala ()
debug = do
    Env env <- ask
    ks <- lift get
    let f env p = liftIO $ forM_ env $ \(x, ref, _) -> do
            thk <- readIORef ref
            putStrLn $ p ++ "Debug Env _ " ++ x ++ " = " ++ show thk
        g env p = liftIO $ forM_ env $ \(x, ref, Env env') -> do
            thk <- readIORef ref
            putStrLn $ p ++ "Debug Env _ " ++ x ++ " = " ++ show thk
            f env' " * * * "
    g env ""
    liftIO $ forM_ ks $ \k -> do
        case k of
            CAppL e (Env env) -> do
                putStrLn $ "Debug Cont _ Nested Env" 
                f env " * * * "
                putStrLn $ "Debug Cont _ " ++ " CAppL " ++ show e
            CSave _ -> putStrLn "Debug Cont _ Save"-}

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

useful :: Expr -> Env -> Koala Env
useful expr (Env env) = Env <$> aux (varsOfExpr expr) env
    where aux _ [] = return []
          aux vars (d@(ref, _):env') = do
              decl <- liftIO $ readIORef ref
              case decl of
                  DRaw bnds _ _ | any (`elem` bnds) vars -> do
                      ds <- aux vars env'
                      return $ d : ds
                  DBnd bnds -> 
                      let bnds' = map fst bnds in 
                        if any (`elem` bnds') vars
                            then do
                                ds <- aux vars env'
                                return $ d : ds
                            else aux vars env'
                  _ -> aux vars env'

raise :: Error -> Koala a
raise = lift . lift . left

defer :: Cont -> Koala ()
defer k = do
    env <- ask
    ks <- lift get
    lift $ put (k:ks)

cont :: Koala (Maybe Cont)
cont = do
    ks <- lift get
    case ks of
        [] -> return Nothing
        (k:ks) -> do
            lift $ put ks
            return $ Just k

push :: Pattern -> Expr -> Env -> Koala a ->  Koala a
push x expr env s = do
    let vars = varsOfPattern x
    uenv <- useful expr env
    ref <- liftIO $ newIORef $ DRaw vars x expr
    local (\(Env env') -> Env $ (ref, uenv) : env') s

pushr :: [(Pattern, Expr)] -> Env -> Koala a ->  Koala a
pushr ds env s = do
    denv <- forM ds $ \(x, expr) -> do
        let vars = varsOfExpr expr
        uenv <- useful expr env
        ref <- liftIO $ newIORef $ DRaw vars x expr
        return (ref, uenv)
    let env' = map (\(ref, Env uenv) -> (ref, Env $ env' ++ uenv)) denv
    local (\(Env env) -> Env $ env' ++ env) s
{-
push :: String -> Expr -> Env -> Koala a ->  Koala a
push x expr env s = do
    ref <- liftIO $ newIORef $ TRaw expr
    local (\(Env env') -> Env $ (x, ref, env) : env') s

pushr :: [(String, Expr)] -> Env -> Koala a ->  Koala a
pushr ds env s = do
    denv <- forM ds $ \(x, expr) -> do
        ref <- liftIO $ newIORef $ TRaw expr
        return (x, ref, useful expr env)
    let env' = map (\(x, ref, Env uenv) -> (x, ref, Env $ env' ++ uenv)) denv
    local (\(Env env) -> Env $ env' ++ env) s
-}

unify :: Env -> Pattern -> Expr -> Koala [(String, IORef Thunk)]
unify uenv x expr = case x of
    PWdc -> return []
    PVar x -> do
        ref' <- liftIO $ newIORef (TRaw expr)
        return [(x, ref')]
    PCpl xs -> do
        thk <- local (const uenv) $ whnfExpr expr
        liftIO $ print "Failed here"
        case thk of
            TCpl thks env' | length xs == length thks -> do
                bnds <- zipWithM (unify env') xs thks 
                return $ concat bnds
            _ -> raise MatchError

find :: String -> Koala (IORef Thunk, Env)
find x = do
    Env env <- ask
    debug env
    let aux [] = raise $ UnboundVar x
        aux l@((ref, uenv):env') = do
            decl <- liftIO $ readIORef ref
            case decl of
                DBnd ds -> do
                    liftIO $ print "Hem"
                    liftIO $ print $ map fst ds
                    case safeHead $ filter ((x ==) . fst) ds of
                        Nothing -> aux env'
                        Just (_, ref') -> return (ref', uenv)
                DRaw vs y expr | x `elem` vs -> do
                    liftIO $ print "Ho"
                    bnds <- unify uenv y expr
                    liftIO $ print "/Ho"
                    liftIO $ writeIORef ref (DBnd bnds)
                    aux l
                _ -> aux env'
    aux env <* debug env

evalOp :: EAop -> (Int -> Int -> Int)
evalOp Add = (+)
evalOp Min = (-)
evalOp Mul = (*)
evalOp Div = div
evalOp Mod = mod
evalOp Pow = (^)
evalOp And = \x y -> if (x /= 0) && (y /= 0) then y else 0 
evalOp Or  = \x y -> if x /= 0 then x else y
evalOp Le  = \x y -> if x < y then 1 else 0
evalOp Leq = \x y -> if x <= y then 1 else 0
evalOp Ge  = \x y -> if x > y then 1 else 0
evalOp Geq = \x y -> if x >= y then 1 else 0
evalOp Eq  = \x y -> if x == y then 1 else 0
evalOp Df  = \x y -> if x /= y then 1 else 0

whnfCont :: Thunk -> Koala Thunk
whnfCont thk = do
    k <- cont
    case k of
        Nothing -> return thk
        Just p -> case (thk, p) of
                    (_, CSave ref) -> do
                        liftIO $ writeIORef ref thk
                        whnfCont thk
                    (TVal (VCst k), CIte e1 e2 env) -> do
                        if k /= 0
                            then local (const env) $ whnfExpr e1
                            else local (const env) $ whnfExpr e2
                    _ -> raise ValueError

whnfExpr :: Expr -> Koala Thunk
whnfExpr = \case
    ECst k -> whnfCont $ TVal $ VCst k
    EFun x e -> whnfCont $ TVal $ VFun x e
    EVar x -> whnfVar x
    ECpl es' -> do
        env <- ask
        whnfCont $ TCpl es' env
    EApp e1 e2 -> do
        env <- ask
        defer $ CApp e2 env
        whnfExpr e1
    ELet ds e2 -> do
        env <- ask
        pushr ds env $ whnfExpr e2
    EAex op e1 e2 -> TVal <$> evalExpr (EAex op e1 e2) 
    EIte e1 e2 e3 -> do
        env <- ask
        defer $ CIte e2 e3 env
        whnfExpr e1

whnfVar :: String -> Koala Thunk
whnfVar x = do
    (ref, env) <- find x
    thk <- liftIO $ readIORef ref 
    case thk of
        TRaw expr -> do
            liftIO $ putStrLn $ "[WHNF] New " ++ x
            defer $ CSave ref
            local (const env) $ whnfExpr expr
        _ -> (liftIO $ putStrLn $ "[WHNF] Reused " ++ x) >> whnfCont thk

evalVar :: String -> Koala Value
evalVar x = do
    (ref, env) <- find x
    thk <- liftIO $ readIORef ref
    case thk of
        TVal v -> (liftIO $ putStrLn $ "Reused " ++ x ++ " / " ++ show v) >> evalCont v
        TRaw expr -> do
            liftIO $ putStrLn $ "New " ++ x
            defer $ CSave ref
            local (const env) $ evalExpr expr
        TCpl (thk:thks) env' -> do
            liftIO $ putStrLn $ "Partial " ++ x
            defer $ CSave ref
            defer $ CCpl thks [] env'
            local (const env) $ evalExpr thk

evalCont :: Value -> Koala Value
evalCont v = do
    k <- cont
    case k of
        Nothing -> return v
        Just p -> case (v,p) of
            (_, CSave ref) -> do
                liftIO $ writeIORef ref (TVal v)
                evalCont v
            (VCst k2, CLhs op k1) ->
                evalCont $ VCst $ evalOp op k1 k2
            (VCst k1, CRhs op e2 env) -> do
                defer $ CLhs op k1
                local (const env) $ evalExpr e2
            (VCst k, CIte e1 e2 env) -> do
                if k /= 0
                    then local (const env) $ evalExpr e1
                    else local (const env) $ evalExpr e2
            (VFun x e, CApp e' env) -> push x e' env $ evalExpr e
            (_, CCpl [] vs _) -> evalCont $ VCpl $ reverse (v:vs)
            (_, CCpl (e:es) vs env) -> do
                defer $ CCpl es (v:vs) env
                local (const env) $ evalExpr e
            _ -> raise ValueError

evalExpr :: Expr -> Koala Value
evalExpr = \case
    ECst k -> evalCont $ VCst k
    EFun x e -> evalCont $ VFun x e
    EVar x -> evalVar x
    ECpl es' -> do
        let (e:es) = es'
        env <- ask
        defer $ CCpl es [] env
        evalExpr e
    EApp e1 e2 -> do
        env <- ask
        defer $ CApp e2 env
        evalExpr e1
    ELet ds e2 -> do
        env <- ask
        pushr ds env $ evalExpr e2
    EAex op e1 e2 -> do
        env <- ask
        defer $ CRhs op e2 env
        evalExpr e1 
    EIte e1 e2 e3 -> do
        env <- ask
        defer $ CIte e2 e3 env
        evalExpr e1

runKoala :: Env -> Koala a -> IO (Either Error a)
runKoala env k = do
    s <- runEitherT $ runStateT (runReaderT k env) []
    return $ fst <$> s

whnf :: [(String, Value)] -> Expr -> IO (Either Error Thunk)
whnf prelude expr = do
    env <- forM prelude $ \(x,v) -> do
        ref <- newIORef (TVal v)
        return (x, ref)
    bnd <- newIORef $ DBnd env
    runKoala (Env [(bnd, Env [])]) $ whnfExpr expr

eval :: [(String, Value)] -> Expr -> IO (Either Error Value)
eval prelude expr = do
    env <- forM prelude $ \(x,v) -> do
        ref <- newIORef (TVal v)
        return (x, ref)
    bnd <- newIORef $ DBnd env
    runKoala (Env [(bnd, Env [])]) $ evalExpr expr