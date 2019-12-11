module Types.TypeError where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Map ((!))
import qualified Data.Map as Map

import BNFC.AbsLatte hiding (Int, Void, Bool)
import qualified BNFC.AbsLatte as Latte
import Types.Abs

-- has to be here to be used in both TypeError.hs and Typecheck.hs
getType :: Ident -> TypeM Type
getType i = do
  typeMap <- get
  unless (i `Map.member` typeMap) $ identNotDeclared i
  return $ typeMap ! i

typeError :: TypeError -> TypeM ()
typeError te = do
  (_, log) <- ask
  throwError $ show te ++ "\n" ++ printlog log

printlog :: [String] -> String
printlog = unlines . map ("Inside " ++)

typeNotFound :: Ident -> TypeM ()
typeNotFound = typeError . TypeNotFound

identNotDeclared :: Ident -> TypeM ()
identNotDeclared = typeError . IdentNotDeclared

typeMismatch :: Type -> Type -> TypeM ()
typeMismatch t1 t2 = typeError $ TypeMismatch t1 t2

mustBeClass :: Type -> TypeM ()
mustBeClass = typeError . MustBeClass

memberNotFound :: Ident -> Ident -> TypeM ()
memberNotFound objid memid = do
  t <- getType objid
  case t of
    Struct i -> typeError $ MemberNotFound objid i memid
    _ -> error "this can never happen"

nonFunctionType :: Ident -> Type -> TypeM ()
nonFunctionType i t = typeError $ NonFunctionType i t

invalidExprType :: Expr -> Type -> TypeM ()
invalidExprType e t = typeError $ InvalidExprType e t

duplicateArgNames :: Ident -> TypeM ()
duplicateArgNames = typeError . DuplicateArgNames

redeclaration = typeError . Redeclaration

noReturn = typeError NoReturn
