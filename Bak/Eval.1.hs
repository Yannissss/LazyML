{-# LANGUAGE LambdaCase #-}
module Eval where

import           Control.Applicative (liftA2)
import           Control.Monad (void, forM, mapM_, (>>))
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List (delete)

import Debug.Trace

import qualified Data.Vector as V

import           Expr

data Val = VCst Int
         | VFun String Expr
         deriving (Eq, Show, Ord)

instance Prettify Val where
    prettify = \case
        VCst k   -> show k
        VFun _ _ -> "<fun>"

data Thunk = TVal Val
           | TRaw Expr REnv
           | TRef Int
           deriving (Eq, Show, Ord)

data RError = ValueError
            | UnboundVar String
            | NoMatch
            | PanicError
            deriving (Eq, Show, Ord)

type REnv = [(String, Int)]
type RMem = V.Vector Thunk
type Red = ReaderT REnv (StateT RMem (Either RError))

reduceToValOp :: EAop -> (Int -> Int -> Int)
reduceToValOp Add = (+)
reduceToValOp Min = (-)
reduceToValOp Mul = (*)
reduceToValOp Div = div
reduceToValOp Mod = mod
reduceToValOp And = \x y -> if (x /= 0) && (y /= 0) then y else 0 
reduceToValOp Or  = \x y -> if x /= 0 then x else y
reduceToValOp Le  = \x y -> if x < y then 1 else 0
reduceToValOp Leq = \x y -> if x <= y then 1 else 0
reduceToValOp Ge  = \x y -> if x > y then 1 else 0
reduceToValOp Geq = \x y -> if x >= y then 1 else 0
reduceToValOp Eq  = \x y -> if x == y then 1 else 0
reduceToValOp Df  = \x y -> if x /= y then 1 else 0

debug :: (Show a) => a -> Red ()
debug x = trace (">> " ++ show x) $ return ()

err :: RError -> Red a
err = lift . lift . Left

buildEnv :: [(String, Val)] -> Red REnv
buildEnv = mapM $ \(x, v) -> do
    ref <- save $ TVal v
    return (x, ref)

usefulEnv :: Expr -> REnv -> REnv
usefulEnv e env = aux (varsOfExpr e) env
    where aux p [] = []
          aux p (q@(y,ref):qs)
            | y `elem` p = q : aux (delete y p) qs
            | otherwise  = aux p qs

alloc :: Thunk -> Red Int
alloc thk = do
    mem <- lift get
    let ref = V.length mem
    lift $ put $ V.snoc mem thk
    return ref

push :: Expr -> Red Int
push e = do
    env <- ask
    mem <- lift get
    debug "push"
    debug (env,mem)
    let uenv = usefulEnv e env
    alloc (TRaw e' uenv)

pushr :: Expr -> Red Int
pushr e = do
    env <- ask
    mem <- lift get
    debug "push"
    debug (env,mem)
    let uenv = usefulEnv e ((x, V.length mem) : env)
    alloc (TRaw e' uenv)

find :: String -> Red Int
find x = do
    env <- ask
    mem <- lift get
    debug "find"
    debug x
    debug (env, mem)
    case lookup x env of
        Nothing  -> err $ UnboundVar x
        Just ref -> return ref

repr :: Int -> Red Int
repr ref = do
    mem <- lift get
    (ref', mem') <- aux ref mem
    put mem'
    return ref'
    where aux ref mem = do
            (thk, lst) <- V.indexM mem ref
            case thk of
                TRef ref' -> do
                    (rref, rmem) <- aux ref' mem
                    return (rref, mem V.// [(ref, (TRef rref, lst))])
                _ -> return (ref, mem)

update :: Int -> Thunk -> Red ()
update ref thk = do
    mem <- lift get
    (_, env) <- V.indexM mem ref
    lift $ put $ mem V.// [(ref, (thk, env))]

reduceToValI :: Int -> Red Val
reduceToValI ref = do
    ref' <- repr ref
    mem <- lift get
    (thk, env) <- V.indexM mem ref'
    case thk of
        TRef _ -> err PanicError
        TVal v -> return v
        TRaw e uenv -> do
            let e' = foldr (\(x,ref) e -> subst x (EThk ref) e) e uenv
            v <- local (const []) $ reduceToVal e
            update ref (TVal v)
            return v

reduceToVal :: Expr -> Red Val
reduceToVal = \case
    ECst k -> return $ VCst k
    EVar x -> do
        ref <- find x
        v <- reduceToValI ref
        return v
    EThk ref -> do
        v <- reduceToValI ref
        return v
    EFun x e -> do
        env <- ask
        let uenv = usefulEnv e env
        return $ VFun x e uenv
    ELet x e1 e2 -> do
        ref <- pushr e1
        let e' = subst x (EThk ref) e2
    EAex op e1 e2 -> do
        lhs <- reduceToVal e1
        rhs <- reduceToVal e2
        case (lhs, rhs) of
            (VCst k1, VCst k2) -> return $ VCst $ reduceToValOp op k1 k2
            _ -> err ValueError
    EApp e1 e2 -> do
        v1 <- reduceToVal e1
        case v1 of 
            VFun x e -> do
                pushr x e2 $ reduceToVal e
            _ -> err ValueError
    EIte eb e1 e2 -> do
        v <- reduceToVal eb
        case v of
            VCst t ->
                if t /= 0
                    then reduceToVal e1
                    else reduceToVal e2
            _ -> err ValueError

eval :: [(String, Val)] -> Expr -> Either RError Val
eval env expr = fst <$> runStateT (runReaderT (do
    env <- buildEnv env
    local (const env) $ reduceToVal expr
    ) []) V.empty