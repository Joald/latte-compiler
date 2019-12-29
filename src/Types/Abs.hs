{-# LANGUAGE
  FlexibleInstances
, MultiParamTypeClasses
, UndecidableInstances
, ConstraintKinds
, FlexibleContexts #-}
module Types.Abs where

import Prelude hiding (id)

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import BNFC.AbsLatte hiding (Int, Void, Bool)
import qualified BNFC.AbsLatte as Latte
import BNFC.PrintLatte


data Class = Class
  { className :: Ident
  , classBase :: Ident
  , classMembers :: Map Ident Type
  } deriving (Eq, Ord, Show, Read)

type TypeMap = Map Ident Type
type ClassMap = Map Ident Class
type TypeDict = (TypeMap, ClassMap)

type TypeM =
  ExceptT String
   (ReaderT RT
    (State TypeMap))

type RT = (Type, [String], ClassMap)


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
  | LoopInInheritance [Ident]
  | WrongNumberOfArgs Int Int
  | NoMain
  | VoidArg Ident
  | InvalidMainType Type
  | RedefinedFunction Ident
  | VoidVariable Ident
  | FunctionNotCalled Ident
  | FieldOverride Ident
  | MethodOverload Ident
  | IntegerConstantTooLarge Integer

instance Show TypeError where
  show (TypeNotFound (Ident id)) = "type " ++ id ++ " not found"
  show (TypeMismatch t1 t2) = "type mismatch: Expected " ++ printTree t2 ++ ", got " ++ printTree t1
  show (IdentNotDeclared (Ident id)) = "undeclared identifier " ++ id
  show (MustBeClass t) = "Type " ++ printTree t ++ " must be a class"
  show (MemberNotFound (Ident objid) (Ident clsid) (Ident memid)) = "Object " ++ objid ++ " is of class " ++ clsid ++ " which has no members named " ++ memid
  show (NonFunctionType (Ident id) t) = "Identifier " ++ id ++ " was called, but is of non-function type " ++ printTree t
  show (InvalidExprType e t) = "Cannot assign type " ++ printTree t ++ " to expression " ++ printTree e
  show (DuplicateArgNames (Ident id)) = "Duplicate argument name " ++ id
  show (Redeclaration (Ident id)) = "Redeclaration of identifier " ++ id
  show (NoReturn) = "No return statement in function returning non-void"
  show (LoopInInheritance s) = "Loop in inheritance " ++ intercalate " --> " (map (\(Ident x) -> x) $ reverse s)
  show (WrongNumberOfArgs i1 i2) = "Wrong number of arguments passed, expected " ++ show i1 ++ ", got " ++ show i2
  show (NoMain) = "No main function found."
  show (VoidArg (Ident id)) = "Invalid type void for argument " ++ id
  show (InvalidMainType t) = "Invalid type " ++ printTree t ++ " for the main function"
  show (RedefinedFunction (Ident id)) = "Redefinition of function " ++ id
  show (VoidVariable (Ident id)) = "Declaration with type void of identifier " ++ id
  show (FunctionNotCalled (Ident id)) = "Function " ++ id ++ " not called"
  show (FieldOverride (Ident id)) = "Field named " ++ id ++ " already exists in a base class"
  show (MethodOverload (Ident id)) = "Method named " ++ id ++ "already defined with different type."
  show (IntegerConstantTooLarge i) = "Integer constant " ++ show i ++ " out of bounds, must be in [2^31, 2^31 - 1]"

