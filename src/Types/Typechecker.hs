{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Types.Typechecker where

import Prelude hiding (id) -- to avoid shadowing warnings

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe

import BNFC.AbsLatte hiding (Int, Bool, Void, Any)
import qualified BNFC.AbsLatte as Latte
import BNFC.PrintLatte
import Utils
import Types.Abs
import Types.TypeError
import Types.Utils

typeCheck :: Program -> Either String ClassMap
typeCheck (Program topDefs) =
  let cls = collectClasses topDefs
  in runTypeMWithClassMap (checkTopDefs topDefs) cls >> return cls

checkTopDefs :: [TopDef] -> TypeM TypeDict
checkTopDefs topDefs = do
  checkClassMap
  let fnames = collectFuns topDefs
  let fs = Map.fromList fnames
  unless (Map.member (Ident "main") fs) noMain
  let t@(Fun retType argTypes) = fs ! Ident "main"
  unless (retType == Latte.Int && null argTypes) $ invalidMainType t
  dups <- duplicates . (map fst fnames ++) <$> gets Map.keys
  unless (null dups) $ redefinedFunction (head dups)
  modify $ Map.union fs
  mapM_ checkTopDef topDefs
  (,) <$> get <*> getClassMap

checkClassMap :: TypeM ()
checkClassMap = do
  m <- getClassMap
  let res = findCycles m
  unless (isNothing res) $ typeError $ fromJust res

findCycles :: ClassMap -> Maybe TypeError
findCycles m = go (Map.toList m)
  where
    go ((id, _):xs) = evalState (go' id) [] !-! go xs
    go [] = Nothing
    go' id = ifM (elem id <$> get) (Just . LoopInInheritance . (id:) <$> get) $ do
      modify (id:)
      let Class _ b _ = m ! id
      if b == noBaseClass
        then return Nothing
        else go' b

collectClasses :: [TopDef] -> ClassMap
collectClasses = Map.fromList . go
  where
    go (TopFnDef _:tds) = go tds
    go (StrDef id inh mems:tds) =
      (id, Class { className = id
                 , classBase = inhToIdent inh
                 , classMembers = mems'}) : go tds
      where mems' = Map.fromList $ map getMember mems
    go [] = []

collectFuns :: [TopDef] -> [(Ident, Type)]
collectFuns (StrDef _ _ _:tds) = collectFuns tds
collectFuns (TopFnDef fn@(FnDef _ id _ _):tds) = (id, funDefType fn) : collectFuns tds
collectFuns [] = []

checkTopDef :: TopDef -> TypeM ()
checkTopDef (StrDef id@(Ident name) inh mems) = enter ("class " ++ name) $ do
  checkInheritance inh
  let mems' = Map.fromList $ map getMember mems
  let dups = duplicates (Map.keys mems')
  unless (null dups) $ redeclaration (head dups)
  st <- get
  modify $ Map.union mems'
  modify $ Map.insert (Ident "self") (Struct id)
  mapM_ (validateMember id) mems
  put st
checkTopDef (TopFnDef fn) = checkFnDef fn

validateMember :: Ident -> MemberDecl -> TypeM ()
validateMember clsid (MethDecl fn) = do
  checkOverride clsid fn
  checkFnDef fn
validateMember clsid (FieldDecl t id) = do
  checkBaseMembers clsid id
  checkVoidness t id
  validateType t

checkOverride :: Ident -> FnDef -> TypeM ()
checkOverride clsid fndef = do
  Class _ base _ <- getClassId clsid
  _checkOverride base fndef
  where
    _checkOverride clsid fndef@(FnDef _ fnid _ _) = unless (clsid == noBaseClass) $ do
      Class _ base mems <- getClassId clsid
      when (fnid `Map.member` mems && mems ! fnid /= funDefType fndef) $ methodOverload fnid
      _checkOverride base fndef

checkBaseMembers :: Ident -> Ident -> TypeM ()
checkBaseMembers clsid fldid = do
  Class _ base _ <- getClassId clsid
  _checkBaseMembers base fldid
  where
    _checkBaseMembers clsid fldid = unless (clsid == noBaseClass) $ do
      Class _ base mems <- getClassId clsid
      when (fldid `Map.member` mems) $ fieldOverride fldid
      _checkBaseMembers base fldid


checkInheritance :: Inheritance -> TypeM ()
checkInheritance NoInherit = return ()
checkInheritance (Extends id) = enter "inheritance clause" $ do
  cls <- getClassMap
  unless (Map.member id cls) $ typeNotFound id

checkFnDef :: FnDef -> TypeM ()
checkFnDef (FnDef t id args bl@(Block stmts)) = enterFunction id t $ do
  validateType t
  let dups = duplicates $ map (\(Arg _ i) -> i) args
  unless (null dups) $ duplicateArgNames (head dups)
  st <- get
  mapM_ registerArg args
  checkStmt (BStmt bl)
  put st
  unless (t == Latte.Void) $ checkRet stmts


checkRet :: [Stmt] -> TypeM ()
checkRet stmts = unless (go stmts) noReturn
  where
    go = any go'
    go' (Ret _) = True
    go' (BStmt (Block stmts')) = go stmts'
    go' (Cond cond stmt) = isTrivially True cond && go' stmt
    go' (CondElse cond stmt1 stmt2) =
      isTrivially True cond && go' stmt1 ||
      isTrivially False cond && go' stmt2 ||
      go' stmt1 && go' stmt2
    go' (While cond stmt) = isTrivially True cond && go' stmt
    go' _ = False

registerArg :: Arg -> TypeM ()
registerArg (Arg t id) = do
  when (t == Latte.Void) $ voidArg id
  modify $ Map.insert id t

enterFunction :: Ident -> Type -> TypeM () -> TypeM ()
enterFunction (Ident name) t = enter ("function " ++ name) . local (first3 $ const t)

enter :: String -> TypeM a -> TypeM a
enter s = local $ second3 (s:)

validateType :: Type -> TypeM ()
validateType (Struct id) = do
  valid <- existsClass id
  unless valid $ typeNotFound id
validateType _ = return ()

currentRetType :: TypeM Type
currentRetType = asks fst3

checkStmt :: Stmt -> TypeM ()
checkStmt s = enter ("statement " ++ printTree s) (_checkStmt s)
  where
    _checkStmt (BStmt (Block stmts)) = enter "block" $ do
      let dups = duplicates $ concatMap getIdentDecls stmts
      unless (null dups) $ redeclaration $ fst $ head dups
      checkBlock stmts
    _checkStmt (Decl t items) = mapM_ (validateItem t) items
    _checkStmt (Ass id expr) = do
      t <- getType id
      void $ checkExpr expr t
    _checkStmt (Incr id) = void $ id `hasType` Latte.Int
    _checkStmt (Decr id) = void $ id `hasType` Latte.Int
    _checkStmt (Ret expr) = do
      t <- currentRetType
      checkVoidness t (Ident "return value")
      void $ checkExpr expr t
    _checkStmt Empty = return ()
    _checkStmt (MemAss objid memid expr) = do
      t <- checkMembership objid memid
      void $ checkExpr expr t
    _checkStmt VRet = void $ currentRetType >>= mustBeType Latte.Void
    _checkStmt (Cond cond stmt) = checkExpr cond Latte.Bool >> checkStmt stmt
    _checkStmt (CondElse cond stmt elseStmt) = checkExpr cond Latte.Bool >> checkStmt stmt >> checkStmt elseStmt
    _checkStmt (While cond stmt) = checkExpr cond Latte.Bool >> checkStmt stmt
    _checkStmt (StExp expr) = void $ checkExpr expr Latte.Any

hasType :: Ident -> Type -> TypeM Type
hasType id t = do
  t' <- getType id
  t' `mustBeType` t
  when (isFunction t') $ functionNotCalled id
  return t'

isBaseOf :: Type -> Type -> TypeM Bool
tSub `isBaseOf` tBase =
  if tSub == tBase then return True else do
    base <- classBase <$> getClassType tSub
    if base == noBaseClass
      then return False
      else Struct base `isBaseOf` tBase


mustBeType :: Type -> Type -> TypeM Type
Latte.Any `mustBeType` t = return t
t `mustBeType` Latte.Any = return t
t1@(Struct _) `mustBeType` t2@(Struct _) =
  ifM ((||) <$> t1 `isBaseOf` t2 <*> t2 `isBaseOf` t1) (return t1) (typeMismatch t1 t2)
t1 `mustBeType` t2 = unless (t1 == t2) (typeMismatch t1 t2) >> return t1

getClassType :: Type -> TypeM Class
getClassType (Struct id) = getClassId id
getClassType t = mustBeClass t >> return dummyClass

getClassId :: Ident -> TypeM Class
getClassId id = do
  classMap <- getClassMap
  unless (id `Map.member` classMap) $ typeNotFound id
  return $ classMap ! id

checkBlock :: [Stmt] -> TypeM ()
checkBlock (stmt:stmts) =
  case stmt of
    Decl t items -> do
      items' <- mapM (validateItem t) items
      -- this means multiple declarations in one statement cannot depend on each other
      modify $ Map.union $ Map.fromList items'
      checkBlock stmts
    _ -> checkStmt stmt >> checkBlock stmts
checkBlock [] = return ()


validateItem :: Type -> Item -> TypeM (Ident, Type)
validateItem t (NoInit id) = checkVoidness t id >> return (id, t)
validateItem t (Init id expr) = do
  checkVoidness t id
  checkExpr expr t
  return (id, t)

checkVoidness :: Type -> Ident -> TypeM ()
checkVoidness t id = when (t == Latte.Void) $ voidVariable id

existsClass :: Ident -> TypeM Bool
existsClass id = Map.member id <$> getClassMap

getClassMembers :: Ident -> TypeM TypeMap
getClassMembers objid = classMembers <$> (getType objid >>= getClassType)

checkExpr :: Expr -> Type -> TypeM Type
checkExpr _e _t = enter ("expression " ++ printTree _e) (_checkExpr _e _t)
  where
    _checkExpr :: Expr -> Type -> TypeM Type
    _checkExpr (EVar id) t = hasType id t
    _checkExpr (ELitInt c) t = checkBounds c >> Latte.Int `mustBeType` t
    _checkExpr ELitTrue t = Latte.Bool `mustBeType` t
    _checkExpr ELitFalse t = Latte.Bool `mustBeType` t
    _checkExpr ENull t@(Struct _) = return t
    _checkExpr ENull t = mustBeClass t
    _checkExpr (ENew id) t = Struct id `mustBeType` t
    _checkExpr (EApp id exprs) t = do
      ft <- getType id
      case ft of
        (Fun resType argTypes) -> do
          resType `mustBeType` t
          checkCall exprs argTypes
          return resType
        _ -> nonFunctionType id ft
    _checkExpr (EString _) t = Str `mustBeType` t
    _checkExpr (ECast id) t = Struct id `mustBeType` t
    _checkExpr (EAcc objid memid) t = do
      t' <- checkMembership objid memid
      when (isFunction t') $ functionNotCalled memid
      t' `mustBeType` t
    _checkExpr (EMeth objid methid exprs) t = do
      ft <- checkMembership objid methid
      unless (isFunction ft) $ nonFunctionType methid ft
      let Fun retType argTypes = ft
      checkCall exprs argTypes
      retType `mustBeType` t
    _checkExpr (Neg expr) t = Latte.Int `mustBeType` t >> checkExpr expr Latte.Int
    _checkExpr (Not expr) t = Latte.Bool `mustBeType` t >> checkExpr expr Latte.Bool
    _checkExpr (EMul e1 _ e2) t = Latte.Int `mustBeType` t >> checkBin Latte.Int e1 e2
    _checkExpr (EAdd e1 op e2) t = do
      t1 <- checkExpr e1 Latte.Any
      if op == Plus && t1 == Str
        then Str `mustBeType` t >> checkExpr e2 Str
        else do
        Latte.Int `mustBeType` t
        Latte.Int `mustBeType` t1
        checkExpr e2 Latte.Int
    _checkExpr (ERel e1 op e2) t = do
      Latte.Bool `mustBeType` t
      if op `elem` [EQU, NE]
        then do
          t1 <- checkExpr e1 Latte.Any
          checkExpr e2 t1
        else checkBin Latte.Int e1 e2
    _checkExpr (EAnd e1 e2) t = Latte.Bool `mustBeType` t >> checkBin Latte.Bool e1 e2
    _checkExpr (EOr  e1 e2) t = Latte.Bool `mustBeType` t >> checkBin Latte.Bool e1 e2

checkBounds :: Integer -> TypeM ()
checkBounds i = when (i < -2^31 || 2^31 - 1 < i) $ integerConstantTooLarge i 

checkBin :: Type -> Expr -> Expr -> TypeM Type
checkBin (Latte.Any) _ _ = error "this should never happen"
checkBin t e1 e2 = checkExpr e1 t >> checkExpr e2 t

checkCall :: [Expr] -> [Type] -> TypeM ()
checkCall exprs types = do
  let le = length exprs
  let lt = length types
  unless (le == lt) $ wrongNumberOfArgs lt le
  zipWithM_ checkExpr exprs types

checkMembership :: Ident -> Ident -> TypeM Type
checkMembership objid memid = do
  Class name _ _ <- getType objid >>= getClassType
  _checkMembership name
  where
    _checkMembership clid = do
      Class _ base members <- asks $ (! clid) . thd3
      if memid `Map.member` members
        then return $ members ! memid
        else do
        when (base == noBaseClass) $ memberNotFound objid memid
        _checkMembership base
