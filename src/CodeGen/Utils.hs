{-# LANGUAGE RecordWildCards, NamedFieldPuns, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BlockArguments #-}
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
  getClassMap = asks fst4


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
enterClass name = local (second4 (const name))

getClassOfVar :: Ident -> CodeGen Ident
getClassOfVar name = (\(Struct name) -> name) <$> askMap name frh4

getCurrentClass :: CodeGen Ident
getCurrentClass = asks snd4

getLocMap :: CodeGen LocMap
getLocMap = asks thd4

getFieldOffset :: ClassMappable m => Ident -> Ident -> m Integer
getFieldOffset clsName fldName =
  (+1) . toInteger . fromJust . elemIndex fldName . map fst <$> getAllFields clsName

getIdentDeclsDeep :: Stmt -> [(Ident, Type)]
getIdentDeclsDeep = mapStmt getIdentDecls

type Mangler = ReaderT IdentMap (State Integer)

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

showSection :: Section -> String
showSection = decapitalize . tail . show

showAsm :: Asm -> String
showAsm (ACustom s) = s
showAsm (ASection section asm) =
  "section ." ++ showSection section ++ "\n" ++ intercalate "\n" (map showAsm asm)
showAsm (ALabel (Ident name)) = name ++ ":"
showAsm (ABin op p1 p2) = "  " ++ decapFull (show op) ++ " " ++ showParam p1 ++ ", " ++ showParam p2
showAsm (AUn op p) = "  " ++ decapFull (show op) ++ " " ++ showParam p
showAsm (AConstStr strName str) = "  " ++ strName ++ ": db " ++ show str ++ ", 0"
showAsm (Aequ (Ident l) ls) = "  " ++ l ++ ": dd " ++ intercalate ", " (map unIdent ls)
showAsm Aenter = "  push ebp\n  mov ebp, esp"
showAsm x = "  " ++ tail (show x)

showParam :: Param -> String
showParam (PReg reg) = decapFull $ show reg
showParam (PImm i) = show i
showParam (PAddr base mscale mindex mdispl) =
  let tscale = isJust mscale
      tindex = isJust mindex
      tdispl = isJust mdispl && fromJust mdispl >= 0
      scale = maybeShow mscale
      index = maybeShow mindex
      displ = maybeShow mdispl
      op1 = if tindex || tdispl then "+" else ""
      op2 = if tscale then "*" else ""
      op3 = if tindex && tdispl then "+" else ""
  in "dword ["
     ++ showReg base ++ op1 ++ scale ++ op2 ++ index ++ op3 ++ displ
     ++ "]"
showParam (PLabel (Ident name)) = name
--showParam (PLabelAddr (Ident name)) = "[rel " ++ name ++ "]"
showReg :: Reg -> String
showReg = decapFull . show

mapQuad :: (Val -> [a]) -> Quad -> [a]
mapQuad go q =
  case q of
    QBin  v1 v2 _ v3 -> concatMap go [v1, v2, v3]
    QUn   v1 _ v2    -> go v1 ++ go v2
    QCopy v1 v2      -> go v1 ++ go v2
    QCond v1 _ v2 _  -> go v1 ++ go v2
    QCall v1 _ _ vs  -> go v1 ++ concatMap go vs
    QRet  v          -> go v
    QNew  v _        -> go v
    QFun  _ qs       -> concatMap (mapQuad go) qs
    _                -> []
