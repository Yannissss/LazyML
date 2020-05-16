{-# LANGUAGE LambdaCase #-}
module Env where

import qualified Data.Map as M

import           Expr

type Namespace k a = M.Map k [a]

empty :: Namespace k a
empty = M.empty

using :: (Ord k, Show k, Show a) => k -> a -> Namespace k a -> Namespace k a
using key val = M.insertWith (++) key [val]

retrieve :: (Ord k) => k -> Namespace k a -> Maybe a
retrieve key ns = case M.lookup key ns of
    Just (val:_) -> Just val
    _            -> Nothing

erase :: (Ord k) => k -> Namespace k a -> Namespace k a
erase = M.update $ \case
    []     -> Nothing
    (_:xs) -> Just xs