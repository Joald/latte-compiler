module Types.TypeError where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.Map ((!))
import qualified Data.Map as Map

import BNFC.AbsLatte hiding (Int, Void, Bool)
import Types.Abs

import Utils

-- has to be here to be used in both TypeError.hs and Typecheck.hs
getType :: Ident -> TypeM Type
getType i = do
  typeMap <- get
  unless (i `Map.member` typeMap) $ identNotDeclared i
  return $ typeMap ! i

typeError :: TypeError -> TypeM a
typeError te = do
  logs <- snd3 <$> ask
  throwError $ show te ++ "\n" ++ printlog logs

printlog :: [String] -> String
printlog = unlines . map ("Inside " ++)

typeNotFound :: Ident -> TypeM a
typeNotFound = typeError . TypeNotFound

identNotDeclared :: Ident -> TypeM a
identNotDeclared = typeError . IdentNotDeclared

typeMismatch :: Type -> Type -> TypeM a
typeMismatch t1 t2 = typeError $ TypeMismatch t1 t2

mustBeClass :: Type -> TypeM a
mustBeClass = typeError . MustBeClass

memberNotFound :: Ident -> Ident -> TypeM a
memberNotFound objid memid = do
  t <- getType objid
  case t of
    Struct i -> typeError $ MemberNotFound objid i memid
    _ -> error "this can never happen"

nonFunctionType :: Ident -> Type -> TypeM a
nonFunctionType i t = typeError $ NonFunctionType i t

invalidExprType :: Expr -> Type -> TypeM a
invalidExprType e t = typeError $ InvalidExprType e t

duplicateArgNames :: Ident -> TypeM a
duplicateArgNames = typeError . DuplicateArgNames

redeclaration :: Ident -> TypeM a
redeclaration = typeError . Redeclaration

noReturn :: TypeM a
noReturn = typeError NoReturn

wrongNumberOfArgs :: Int -> Int -> TypeM a
wrongNumberOfArgs x y = typeError $ WrongNumberOfArgs x y

noMain :: TypeM a
noMain = typeError NoMain

voidArg :: Ident -> TypeM a
voidArg = typeError . VoidArg

invalidMainType :: Type -> TypeM a
invalidMainType = typeError . InvalidMainType

redefinedFunction :: Ident -> TypeM a
redefinedFunction = typeError . RedefinedFunction


voidVariable :: Ident -> TypeM a
voidVariable = typeError . VoidVariable

functionNotCalled :: Ident -> TypeM a
functionNotCalled = typeError . FunctionNotCalled

fieldOverride :: Ident -> TypeM a
fieldOverride = typeError . FieldOverride

methodOverload :: Ident -> TypeM a
methodOverload = typeError . MethodOverload

integerConstantTooLarge :: Integer -> TypeM a
integerConstantTooLarge = typeError . IntegerConstantTooLarge
