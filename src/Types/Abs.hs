{-# LANGUAGE
  FlexibleInstances
, MultiParamTypeClasses
, UndecidableInstances
, ConstraintKinds
, FlexibleContexts #-}
module Types.Abs where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Tardis
import Control.Monad.Trans.Tardis (mapTardisT)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import BNFC.AbsLatte hiding (Int, Void, Bool)
import qualified BNFC.AbsLatte as Latte

import Utils

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

type TypeM =
  ExceptT String
  (TardisT TypeDict TypeDict
   (ReaderT RT
    (State TypeMap)))

type RT = (Type, [String])

type TM m = ( MonadTardis TypeDict TypeDict m
            , MonadReader RT m
            , MonadState TypeMap m
            , MonadError String m )

runTypeM :: TypeM () -> Either String TypeDict
runTypeM m =
  let (res, (st, _)) =
        evalState
          (runReaderT
            (runTardisT (runExceptT m) (emptyPair, emptyPair))
            (Latte.Void, []))
          Map.empty
  in res >> return st

emptyPair :: TypeDict
emptyPair = (Map.fromList $ map (first Ident)
                  [ ("printInt", Fun Latte.Void [Latte.Int])
                  , ("printString", Fun Latte.Void [Str])
                  , ("error", Fun Latte.Void [])
                  , ("readInt", Fun Latte.Int [])
                  , ("readString", Fun Str [])]
            , Map.empty)

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

instance (MonadTardis bw fw m) => MonadTardis bw fw (ExceptT e m) where
  getPast = lift getPast
  getFuture = lift getFuture
  sendFuture = lift . sendFuture
  sendPast = lift . sendPast

instance (MonadTardis bw fw m) => MonadTardis bw fw (StateT s m) where
  getPast = lift getPast
  getFuture = lift getFuture
  sendFuture = lift . sendFuture
  sendPast = lift . sendPast

instance (MonadTardis bw fw m) => MonadTardis bw fw (ReaderT r m) where
  getPast = lift getPast
  getFuture = lift getFuture
  sendFuture = lift . sendFuture
  sendPast = lift . sendPast

instance (MonadFix m, MonadState s m) => MonadState s (TardisT bw fw m) where
  get = lift get
  put = lift . put

instance (MonadFix m, MonadReader r m) => MonadReader r (TardisT bw fw m) where
  ask = lift ask
  local = mapTardisT . local
