{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Eval where

import           Control.Applicative (liftA2)
import           Control.Monad (void, forM, mapM_, zipWithM, (>>), (>=>))
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans

import           Data.List

import           Data.IORef

import           Either
import           Expr

data Value = VCst Int
           | VCpl [Value]
           | VLst [Value]
           | VClosure (Thunk -> Koala Value)

instance Show Value where
    show (VCst k)     = show k
    show (VCpl vs)    = "(" ++ intercalate "," (map show vs) ++ ")"
    show (VLst vs)    = "[" ++ intercalate "," (map show vs) ++ "]"
    show (VClosure _) = "<fun>"

data WHNF = WCst Int
          | WClosure (Thunk -> Koala WHNF)
          | WCpl [Thunk]
          | WNil
          | WCons Thunk Thunk

type Thunk = () -> Koala WHNF

instance Show WHNF where
    show (WCst k)        = show k
    show (WClosure _)    = "<fun>"
    show (WCpl thks)     = "(" ++ intercalate "," (map (const "<thk>") thks) ++ ")"
    show WNil            = "[]"
    show (WCons th1 th2) = "<thk>::<thk>" 

data Error = ValueError
           | MatchError
           | UnboundVar String
           | PanicError String
           deriving (Eq, Show, Ord)

data Decl = DRaw [String] Pattern (IORef Thunk)
          | DBnd [(String, IORef Thunk)]

type Env = [IORef Decl]
type Koala = EitherT Error IO

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

try :: Koala a -> (Error -> Koala a) -> Koala a
try k ke = EitherT $ do
    r <- runEitherT k
    case r of
        Left err -> runEitherT (ke err)
        Right x -> return (Right x)

raise :: Error -> Koala a
raise = left

unify :: Pattern -> (IORef Thunk) -> Koala [(String, IORef Thunk)]
unify p ref = case p of
    PVar x -> return [(x, ref)]
    PCpl ps -> do
        forceToWHNF ref
        th <- liftIO $ readIORef ref
        reducee <- th ()
        refs <- case reducee of
            WCpl ths | length ps == length ths -> mapM (liftIO . newIORef) ths
            _ -> raise MatchError
        dss <- zipWithM unify ps refs
        return $ concat dss
    PCons p1 p2 -> do
        forceToWHNF ref
        th <- liftIO $ readIORef ref
        reducee <- th ()
        case reducee of
            WCons th1 th2 -> do
                ref1 <- liftIO $ newIORef th1
                ref2 <- liftIO $ newIORef th2
                ds1 <- unify p1 ref1
                ds2 <- unify p2 ref2
                return $ ds1 ++ ds2
            _ -> raise MatchError
    PNil -> do
        forceToWHNF ref
        th <- liftIO $ readIORef ref
        reducee <- th ()
        case reducee of
            WNil -> return []
            _ -> raise MatchError
    _ -> return []

lookupEnv :: Env -> String -> Koala (IORef Thunk)
lookupEnv env x = case env of
    [] -> raise $ UnboundVar x
    (r:rs) -> do
        d <- liftIO $ readIORef r
        case d of
            DBnd ds -> case lookup x ds of
                Nothing -> lookupEnv rs x
                Just r  -> return r
            DRaw vars p ref ->
                if x `notElem` vars
                    then lookupEnv rs x
                    else do
                        ds <- unify p ref
                        liftIO $ writeIORef r $ DBnd ds
                        lookupEnv (r:rs) x

updateWHNF :: IORef Thunk -> WHNF -> Koala ()
updateWHNF ref reducee = 
    liftIO $ writeIORef ref (\() -> return reducee)

forceToWHNF :: IORef Thunk -> Koala WHNF
forceToWHNF ref = do
    th <- liftIO $ readIORef ref
    reducee <- th ()
    updateWHNF ref reducee
    return reducee

mkClosureWHNF :: Env -> Pattern -> Expr -> Thunk -> Koala WHNF
mkClosureWHNF env x e th = do
    ref <- liftIO $ newIORef th
    dref <- liftIO $ newIORef $ DRaw (varsOfPattern x) x ref
    whnf (dref : env) e

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

whnfToVal :: WHNF -> Koala Value
whnfToVal = \case
    WCst k -> return $ VCst k
    WClosure c -> return $ VClosure $ c >=> whnfToVal 
    WCpl thks ->  VCpl <$> forM thks (\th -> th () >>= whnfToVal)
    WNil -> return $ VLst []
    WCons th1 th2 -> do
        v1 <- th1 () >>= whnfToVal
        v2 <- th2 () >>= whnfToVal
        case v2 of
            VLst vs -> return $ VLst (v1:vs)
            _ -> raise $ PanicError "whnfToVal: Tail is not a list"

eval :: Env -> Expr -> Koala Value
eval env expr = whnf env expr >>= whnfToVal

whnf :: Env -> Expr -> Koala WHNF
whnf env = \case
    ECst k -> return $ WCst k
    EFun x e -> return $ WClosure $ mkClosureWHNF env x e
    EVar x -> lookupEnv env x >>= forceToWHNF
    ECpl es -> return $ WCpl $ map (\e () -> whnf env e) es
    EApp e1 e2 -> do
        v1 <- whnf env e1
        case v1 of
            WClosure c -> c (\() -> whnf env e2)
            _ -> raise $ PanicError "eval: Not callable"
    EAex op e1 e2 -> do
        case op of
            And -> do
                v1 <- whnf env e1
                case v1 of
                    WCst k | k == 0 -> return $ WCst 0
                    WCst _ -> do
                        v2 <- whnf env e2
                        case v2 of
                            WCst k -> return $ WCst k
                            _ -> raise $ PanicError "eval: Expected int to evaluate op"
                    _ -> raise $ PanicError "eval: Expected int to evaluate op"
            Or  -> do
                v1 <- whnf env e1
                case v1 of
                    WCst k | k /= 0 -> return v1
                    WCst _ -> do
                        v2 <- whnf env e2
                        case v2 of
                            WCst k -> return $ WCst k
                            _ -> raise $ PanicError "eval: Expected int to evaluate op"
                    _ -> raise $ PanicError "eval: Expected int to evaluate op"
            _   -> do
                v1 <- whnf env e1
                v2 <- whnf env e2
                case (v1, v2) of
                    (WCst k1, WCst k2) -> return $ WCst $ evalOp op k1 k2
                    _ -> raise $ PanicError "eval: Expected int to evaluate op"
    EIte e1 e2 e3 -> do
        vb <- whnf env e1
        case vb of
            WCst k -> if k /= 0 then whnf env e2 else whnf env e3
            _ -> raise $ PanicError "eval: Expected in to evaluate if"
    EFix e -> whnf env (EApp e (EFix e))
    ENil -> return WNil
    ECons e1 e2 -> return $ WCons (\() -> whnf env e1) (\() -> whnf env e2)
    EMatch e cs -> do
        ref <- liftIO $ newIORef $ (\() -> whnf env e)
        match ref cs
        where match ref [] = raise MatchError
              match ref ((p,e'):cs) = do
                  (flip try) (\_ -> match ref cs) $ do
                        ds <- unify p ref
                        env' <- liftIO $ newIORef $ DBnd ds
                        whnf (env' : env) e'

lazyPrint :: WHNF -> Koala ()
lazyPrint = aux >=> (\() -> liftIO $ putStrLn "")
    where pint [] = return ()
          pint [x] = x
          pint (x:xs) = do
              x
              liftIO $ putStr ","
              pint xs
          aux = \case
                WCst k -> liftIO $ putStr $ show k
                WClosure c -> liftIO $ putStr "<fun>" 
                WCpl thks ->  do
                    liftIO $ putStr "("
                    pint $ map (\th -> th () >>= aux) thks
                    liftIO $ putStr ")"
                WNil -> liftIO $ putStr "[]"
                WCons th1 th2 -> do
                    liftIO $ putStr "["
                    th1 () >>= aux
                    r <- th2 ()
                    case r of
                        WNil -> liftIO $ putStr "]"
                        _ -> do
                            liftIO $ putStr ","
                            aux' r
          aux' = \case
                WCons th1 th2 -> do
                    th1 () >>= aux
                    r <- th2 ()
                    case r of
                        WNil -> liftIO $ putStr "]"
                        _ -> do
                            liftIO $ putStr ","
                            aux' r
                r  -> aux r 