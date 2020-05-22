{-# LANGUAGE LambdaCase #-}
module Eval where

import           Control.Applicative (liftA2)
import           Control.Monad (void, forM, mapM_, (>>))
import           Control.Monad.Reader
import           Control.Monad.State

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
           | TRaw Expr
           | TRef Int
           deriving (Eq, Show, Ord)

data RError = ValueError
            | UnboundVar String
            | NoMatch
            | PanicError
            deriving (Eq, Show, Ord)

type REnv = [String, Int]
type RMem = V.Vector (Thunk, REnv)
type RState = (REnv, RMem)
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
    ref <- save $ ECst 0
    update ref (TVal v)
    return (x, ref)

usefulEnv :: Expr -> REnv -> REnv
usefulEnv e env = aux (varsOfExpr e) env
    where aux p [] = []
          aux p (q@(y,ref):qs) = if y `elem` p then q : (aux (delete y p) qs) else aux p qs

save :: Thunk -> Red Int
save thk e = do
    mem <- lift get
    let ref = V.length mem
    lift $ put $ V.snoc mem thk
    return ref

push :: String -> Expr -> Red a -> Red a
push x e s = case x of
    "_" -> s
    _   -> do
        env <- ask
        let uenv = usefulEnv env
        ref <- save (TRaw e, uenv)
        local ((x, ref) :) s

find :: String -> Red Int
find x = do
    env <- ask
    debug "find"
    debug x
    debug sptr
    case lookup x env of
        Nothing  -> err $ UnboundVar x
        Just ref -> retunr ref
    let aux idx
          | idx < 0   = err $ UnboundVar x :: Red Int
          | otherwise = do
                    lift get >>= debug
                    debug idx
                    (y, ref) <- V.indexM env idx
                    if x == y
                        then return ref
                        else aux (idx-1)
    if sptr >= 0
        then aux (sptr-1)
        else aux (V.length env-1)

repr :: Int -> Red Int
repr ref = do
    (env, mem) <- lift get
    (ref', mem') <- aux ref mem
    put (env, mem')
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
    (env, mem) <- lift get
    (_, lst) <- V.indexM mem ref
    let mem' = mem V.// [(ref, (thk, lst))]
    lift $ put (env, mem')

reduceToValI :: Int -> Red Val
reduceToValI ref = do
    ref' <- repr ref
    (_, mem) <- lift get
    (thk, lst) <- V.indexM mem ref'
    case thk of
        TRef _ -> err PanicError
        TVal v -> return v
        TRaw e -> do
            v <- local (const lst) $ reduceToVal e
            update ref (TVal v)
            return v

reduceToVal :: Expr -> Red Val
reduceToVal = \case
    ECst k -> return $ VCst k
    EVar x -> do
        ref <- find x
        v <- reduceToValI ref
        return v
    EFun x e -> return $ VFun x e
    ELet x e1 e2 -> do
        push x e1
        reduceToVal e2
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
                push x e2
                reduceToVal e
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
eval env expr = fst <$> runStateT (runReaderT (buildEnv env >> reduceToVal expr) (-1)) (V.empty, V.empty)