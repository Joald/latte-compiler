module Types.Typechecker where

import Prelude hiding (id) -- to avoid shadowing warnings

import Control.Monad.Reader
import Control.Monad.Tardis
import Control.Monad.State

import qualified Data.Map as Map
import Data.Map ((!))

import BNFC.AbsLatte hiding (Int, Bool, Void, Any)
import qualified BNFC.AbsLatte as Latte
import Utils
import Types.Abs
import Types.TypeError
import Types.Utils

modifyBothWays :: (TypeDict -> TypeDict) -> TypeM ()
modifyBothWays f = modifyBackwards f >> modifyForwards f

typeCheck :: Program -> Either String TypeDict
typeCheck (Program topDefs) = runTypeM $ checkTopDefs topDefs

checkTopDefs :: [TopDef] -> TypeM ()
checkTopDefs topDefs = mapM_ checkTopDef topDefs

checkTopDef :: TopDef -> TypeM ()
checkTopDef (TopFnDef fn) = do
  checkFnDef fn
  registerFnDef fn

checkTopDef (StrDef id@(Ident name) inh mems) = enter ("class " ++ name) $ do
  mems' <- Map.fromList <$> mapM validateMember mems
  let cl = Class { className = id
                 , classBase = inhToIdent inh
                 , classMembers = mems' }
  addClass id cl

initState :: TypeM ()
initState = do
  (m1, _) <- getFuture
  (m2, _) <- getPast
  put $ m1 `Map.union` m2

deinitState :: TypeM ()
deinitState = put Map.empty

checkFnDef :: FnDef -> TypeM ()
checkFnDef (FnDef t id args bl@(Block stmts)) = enterFunction id t $ do
  validateType t
  let dups = duplicates $ map (\(Arg _ i) -> i) args
  unless (null dups) $ duplicateArgNames (head dups)
  initState
  mapM_ registerArg args
  checkStmt (BStmt bl)
  deinitState
  unless (t == Latte.Void) $ checkRet stmts

checkRet :: [Stmt] -> TypeM ()
checkRet stmts = unless (go stmts) noReturn
  where
    go = any go'
    go' (Ret _) = True
    go' (BStmt (Block stmt)) = go stmt
    go' (CondElse _ stmt1 stmt2) = go' stmt1 && go' stmt2
    go' (While _ stmt) = go' stmt
    go' _ = False

addClass :: Ident -> Class -> TypeM ()
addClass id cl = modifyBothWays $ second $ Map.insert id cl

inhToIdent :: Inheritance -> Ident
inhToIdent NoInherit = Ident ""
inhToIdent (Extends id) = id

registerFnDef :: FnDef -> TypeM ()
registerFnDef fn@(FnDef _ id _ _) =
  modifyBothWays $ first $ Map.insert id $ funDefType fn


funDefType :: FnDef -> Type
funDefType (FnDef ty _ args _) = Fun ty $ map (\(Arg t _) -> t) args

registerArg :: Arg -> TypeM ()
registerArg (Arg t id) = modify $ Map.insert id t
--  changeTypeMap True $ Map.insert id t
--  changeTypeMap False $ Map.delete id

enterFunction :: Ident -> Type -> TypeM () -> TypeM ()
enterFunction (Ident name) t = enter ("function " ++ name) . local (first $ const t)

enter :: String -> TypeM () -> TypeM ()
enter s = local $ second (s:)

validateType :: Type -> TypeM ()
validateType (Struct id) = do
  valid <- existsClass id
  unless valid $ typeNotFound id
validateType _ = return ()

currentRetType :: TypeM Type
currentRetType = fst <$> ask

checkStmt :: Stmt -> TypeM ()
checkStmt s = enter ("statement " ++ show s) (_checkStmt s)
  where
    _checkStmt (BStmt (Block stmts)) = enter ("block " ++ show stmts) $ do
      let dups = duplicates $ getIdentDecls stmts
      unless (null dups) $ redeclaration $ head dups
      checkBlock stmts
    _checkStmt (Decl t items) = mapM_ (validateItem t) items
    _checkStmt (Ass id expr) = do
      t <- getType id
      checkExpr expr t
    _checkStmt (Incr id) = id `hasType` Latte.Int
    _checkStmt (Decr id) = id `hasType` Latte.Int
    _checkStmt (Ret expr) = currentRetType >>= checkExpr expr
    _checkStmt Empty = return ()
    _checkStmt (MemAss objid memid expr) = do
      mems <- getClassMembers objid
      unless (memid `Map.member` mems) $ memberNotFound objid memid
      checkExpr expr $ mems ! memid
    _checkStmt VRet = currentRetType >>= unifyTypes Latte.Void
    _checkStmt (Cond cond stmt) = checkExpr cond Latte.Bool >> checkStmt stmt
    _checkStmt (CondElse cond stmt elseStmt) = checkExpr cond Latte.Bool >> checkStmt stmt >> checkStmt elseStmt
    _checkStmt (While cond stmt) = checkExpr cond Latte.Bool >> checkStmt stmt
    _checkStmt (StExp expr) = checkExpr expr Latte.Any

hasType :: Ident -> Type -> TypeM ()
hasType id t = do
  t' <- getType id
  unifyTypes t t'

unifyTypes :: Type -> Type -> TypeM ()
unifyTypes Latte.Any _ = return ()
unifyTypes _ Latte.Any = return ()
unifyTypes t1 t2 = unless (t1 == t2) $ typeMismatch t1 t2

getClass :: Type -> TypeM Class
getClass (Struct id) = do
  classMap <- getClassMap
  unless (id `Map.member` classMap) $ typeNotFound id
  return $ classMap ! id
getClass t = mustBeClass t >> return dummyClass

checkBlock :: [Stmt] -> TypeM ()
checkBlock (stmt:stmts) = enter ("statement " ++ show stmt) $
  case stmt of
    Decl t items -> do
      items' <- mapM (validateItem t) items
      -- this means multiple declarations in one statement cannot depend on each other
      modify $ Map.union $ Map.fromList items'
      checkBlock stmts
    _ -> checkStmt stmt >> checkBlock stmts
checkBlock [] = return ()

getIdentDecls :: [Stmt] -> [Ident]
getIdentDecls = concatMap extract
  where
    extract (Decl _ items) = map itemName items
    extract _ = []

validateItem :: Type -> Item -> TypeM (Ident, Type)
validateItem t (NoInit id) = return (id, t)
validateItem t (Init id expr) = checkExpr expr t >> return (id, t)

itemName :: Item -> Ident
itemName (NoInit id) = id
itemName (Init id _) = id


validateMember :: MemberDecl -> TypeM (Ident, Type)
validateMember (FieldDecl t id) = return (id, t)
validateMember (MethDecl fn@(FnDef _ id _ _)) = checkFnDef fn >> return (id, funDefType fn)

getClassMap :: TypeM ClassMap
getClassMap = do
  (_, classMap1) <- getPast
  (_, classMap2) <- getFuture
  return $ classMap1 `Map.union` classMap2


existsClass :: Ident -> TypeM Bool
existsClass id = Map.member id <$> getClassMap

getClassMembers :: Ident -> TypeM TypeMap
getClassMembers objid = classMembers <$> (getType objid >>= getClass)

checkExpr :: Expr -> Type -> TypeM ()
checkExpr _e _t = enter ("expression " ++ show _e) (_checkExpr _e _t)
  where
    _checkExpr (EVar id) t = hasType id t
    _checkExpr (ELitInt _) Latte.Int = return ()
    _checkExpr ELitTrue Latte.Bool = return ()
    _checkExpr ELitFalse Latte.Bool = return ()
    _checkExpr ENull (Struct _) = return ()
    _checkExpr ENull t = mustBeClass t
    _checkExpr (ENew id) t = unifyTypes (Struct id) t
    _checkExpr (EApp id exprs) t = do
      ft <- getType id
      case ft of
        (Fun resType argTypes) -> do
          unifyTypes resType t
          zipWithM_ checkExpr exprs argTypes
        _ -> nonFunctionType id ft
    _checkExpr (EString _) t = unifyTypes t Str
    _checkExpr (ECast id) t = unifyTypes (Struct id) t
    _checkExpr (EAcc objid memid) t = do
      members <- getClassMembers objid
      unless (memid `Map.member` members) $ memberNotFound objid memid
      unifyTypes (members ! memid) t
    _checkExpr (EMeth objid methid exprs) t = do
      members <- getClassMembers objid
      unless (methid `Map.member` members) $ memberNotFound objid methid
      let ft@(Fun retType argTypes) = members ! methid
      unless (isFunction ft) $ nonFunctionType methid ft
      unifyTypes t retType
      zipWithM_ checkExpr exprs argTypes
    _checkExpr (Neg expr) t = unifyTypes t Latte.Int >> checkExpr expr t
    _checkExpr (Not expr) t = unifyTypes t Latte.Bool >> checkExpr expr t
    _checkExpr (EMul e1 _ e2) t = unifyTypes t Latte.Int >> checkBin Latte.Int e1 e2
    _checkExpr (EAdd e1 op e2) t =
      if op == Plus && t == Str
      then checkBin Str e1 e2
      else unifyTypes t Latte.Int >> checkBin Latte.Int e1 e2
    _checkExpr (ERel e1 _ e2) t = unifyTypes t Latte.Bool >> checkBin Latte.Int e1 e2
    _checkExpr (EAnd e1 e2) t = unifyTypes t Latte.Bool >> checkBin Latte.Bool e1 e2
    _checkExpr (EOr  e1 e2) t = unifyTypes t Latte.Bool >> checkBin Latte.Bool e1 e2
    _checkExpr e t = invalidExprType e t

checkBin :: Type -> Expr -> Expr -> TypeM ()
checkBin t e1 e2 = checkExpr e1 t >> checkExpr e2 t
