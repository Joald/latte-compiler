{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Types.Utils where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State


import Data.List

import BNFC.AbsLatte hiding (Bool, Int, Void)
import qualified BNFC.AbsLatte as Latte
import Types.Abs
import Utils

dummyClass :: Class
dummyClass = Class (Ident "") (Ident "") Map.empty


runTypeM :: TypeM a -> Either String a
runTypeM m = runTypeMWithClassMap m Map.empty

runTypeMWithClassMap :: TypeM a -> ClassMap -> Either String a
runTypeMWithClassMap m cls =
  evalState (runReaderT (runExceptT m) (Latte.Void, [], cls)) baseMap

baseMap :: TypeMap
baseMap = Map.fromList $ zip builtins
                  [ Fun Latte.Void [Latte.Int]
                  , Fun Latte.Void [Str]
                  , Fun Latte.Void []
                  , Fun Latte.Int []
                  , Fun Str []
                  ]


pairwiseDistinct :: Ord a => [a] -> Bool
pairwiseDistinct = all (null . tail) . group . sort

duplicates :: Ord a => [a] -> [a]
duplicates = concat . filter (not . null) . map tail . group . sort


pairMapUnion :: (Ord k1, Ord k2) => (Map k1 v1, Map k2 v2) -> (Map k1 v1, Map k2 v2) -> (Map k1 v1, Map k2 v2)
pairMapUnion (m1, m2) (m1', m2') = (m1 `Map.union` m1', m2 `Map.union` m2')

isFunction :: Type -> Bool
isFunction (Fun _ _) = True
isFunction _ = False

instance ClassMappable TypeM where
  getClassMap = asks thd3
