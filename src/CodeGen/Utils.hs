{-# LANGUAGE RecordWildCards, NamedFieldPuns, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CodeGen.Utils where

import Control.Monad.Reader
import Control.Monad.State

import Data.Map ((!))
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)

import BNFC.AbsLatte hiding (Int, Bool, Void)

import Utils
import Types.Abs
import CodeGen.Abs


instance ClassMappable CodeGen where
  getClassMap = asks fst3


getClassSize :: Ident -> CodeGen Integer
getClassSize name = do
  (size, baseCount) <- go name
  return $ size + if baseCount > 1 then 8 else 0
  where
    go :: Ident -> CodeGen (Integer, Integer)
    go (Ident "") = return (0, 0)
    go name' = do
      cls <- getClass name'
      (baseSize, baseBaseCount) <- go (className cls)
      let curSize = toInteger (Map.size (classMembers cls))
      return (baseSize + curSize, baseBaseCount + 1)

enterClass :: Ident -> CodeGen a -> CodeGen a
enterClass name = local (second3 (const name))

getCurrentClass :: CodeGen Ident
getCurrentClass = asks snd3

getLocMap :: CodeGen LocMap
getLocMap = asks thd3

getFieldOffset :: ClassMappable m => Ident -> Ident -> m Integer
getFieldOffset clsName fldName =
  toInteger . fromJust . elemIndex fldName . map fst <$> getAllFields clsName

getIdentDeclsDeep :: Stmt -> [Ident]
getIdentDeclsDeep = mapStmt getIdentDecls

type Mangler = ReaderT (Map Ident Ident) (State Integer)

runLocalMangler :: [Stmt] -> [Stmt]
runLocalMangler = runMangler . mangleLocals

runMangler :: Mangler a -> a
runMangler m = evalState (runReaderT m Map.empty) 0

mangleLocals :: [Stmt] -> Mangler [Stmt]
mangleLocals (Decl t items:stmts) = do
  items' <- mapM replaceItem items
  let names = map itemName items'
  names' <- mapM changeIdent names
  let m = Map.fromList $ zip names names'
  items'' <- localUnion m $ mapM replaceItemId items'
  (Decl t items'':) <$> localUnion m (mangleLocals stmts)
mangleLocals (stmt:stmts) = do
  stmt' <- replaceIdent stmt
  stmts' <- mangleLocals stmts
  return $ stmt':stmts'
mangleLocals [] = return []


changeIdent :: Ident -> Mangler Ident
changeIdent (Ident i) = do
  s <- get
  put (s + 1)
  return $ Ident $ i ++ ' ' : show s

replaceIdent :: Stmt -> Mangler Stmt
replaceIdent s =
  case s of
    Ass id e -> Ass <$> mreplace id <*> replaceExpr e
    MemAss id1 id2 e -> MemAss <$> mreplace id1 <*> pure id2 <*> replaceExpr e
    Incr id -> Incr <$> mreplace id
    Decr id -> Decr <$> mreplace id
    Ret e -> Ret <$> replaceExpr e
    Cond e s -> Cond <$> replaceExpr e <*> replaceIdent s
    CondElse e s1 s2 ->
      CondElse <$> replaceExpr e <*> replaceIdent s1 <*> replaceIdent s2
    While e s -> While <$> replaceExpr e <*> replaceIdent s
    StExp e -> StExp <$> replaceExpr e
    BStmt (Block stmts) -> BStmt . Block <$> mangleLocals stmts
    _ -> return s

replaceItem :: Item -> Mangler Item
replaceItem (Init id e) = Init id <$> replaceExpr e
replaceItem e = return e

replaceItemId :: Item -> Mangler Item
replaceItemId (Init id e) = Init <$> mreplace id <*> pure e
replaceItemId (NoInit id) = NoInit <$> mreplace id

replaceExpr :: Expr -> Mangler Expr
replaceExpr e =
  case e of
    EVar id -> EVar <$> mreplace id
    EApp id args -> EApp id <$> mapM replaceExpr args
    EAcc id1 id2 -> EAcc <$> mreplace id1 <*> pure id2
    EMeth id1 id2 args -> EMeth <$> mreplace id1 <*> pure id2 <*> mapM replaceExpr args
    Neg e -> Neg <$> replaceExpr e
    Not e -> Not <$> replaceExpr e
    EMul e1 op e2 -> EMul <$> replaceExpr e1 <*> pure op <*> replaceExpr e2
    EAdd e1 op e2 -> EAdd <$> replaceExpr e1 <*> pure op <*> replaceExpr e2
    ERel e1 op e2 -> ERel <$> replaceExpr e1 <*> pure op <*> replaceExpr e2
    EAnd e1 e2 -> EAnd <$> replaceExpr e1 <*> replaceExpr e2
    EOr e1 e2 -> EOr <$> replaceExpr e1 <*> replaceExpr e2
    _ -> return e
