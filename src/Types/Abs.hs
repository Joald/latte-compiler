{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Types.Abs where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Tardis
import Control.Monad.Trans.Tardis (mapTardisT)
import Control.Monad.Reader
import Control.Monad.Except

import BNFC.AbsLatte

data Class = Class
  { className :: Ident
  , classBase :: Ident
  , classMembers :: Map Ident Type
  } deriving (Eq, Ord, Show, Read)

dummyClass :: Class
dummyClass = Class (Ident "") (Ident "") Map.empty

type TypeMap = Map Ident Type
type ClassMap = Map Ident Class
type TypeDict = (TypeMap, ClassMap)

type TypeM = ExceptT String (TardisT TypeDict TypeDict (Reader (Type, [String])))

data TypeError
  = TypeNotFound Ident
  | TypeMismatch Type Type
  | IdentNotDeclared Ident
  | MustBeClass Type
  | MemberNotFound Ident Ident Ident
  | NonFunctionType Ident Type
  | InvalidExprType Expr Type
  | DuplicateArgNames Ident
  | Redeclaration Ident
  | NoReturn
  deriving (Show)

instance (MonadFix m, MonadReader r m) => MonadReader r (TardisT bw fw m) where
  ask = lift ask
  local = mapTardisT . local
  reader = lift . reader

instance (MonadTardis bw fw m) => MonadTardis bw fw (ExceptT e m) where
  getPast = lift getPast
  getFuture = lift getFuture
  sendFuture = lift . sendFuture
  sendPast = lift . sendPast
