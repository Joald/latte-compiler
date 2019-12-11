module Types.Utils where

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List

import BNFC.AbsLatte hiding (Bool, Int, Void)

pairwiseDistinct :: Ord a => [a] -> Bool
pairwiseDistinct = all (null . tail) . group . sort

duplicates :: Ord a => [a] -> [a]
duplicates = concat . filter (not . null) . map tail . group . sort


pairMapUnion :: (Ord k1, Ord k2) => (Map k1 v1, Map k2 v2) -> (Map k1 v1, Map k2 v2) -> (Map k1 v1, Map k2 v2)
pairMapUnion (m1, m2) (m1', m2') = (m1 `Map.union` m1', m2 `Map.union` m2')

isFunction :: Type -> Bool
isFunction (Fun _ _) = True
isFunction _ = False
