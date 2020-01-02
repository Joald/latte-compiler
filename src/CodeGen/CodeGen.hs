{-# LANGUAGE BlockArguments #-}
module CodeGen.CodeGen where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Map as Map
import Data.Map ((!))

import BNFC.AbsLatte hiding (Int, Void, Bool)
import qualified BNFC.AbsLatte as Latte

import Utils
import Types.Abs
import CodeGen.Abs
import CodeGen.Utils
import CodeGen.QInter

import CodeGen.Asm


codeGen :: ClassMap -> Program -> String
codeGen cls prog =
  unlines . map showAsm
  $ uncurry (quadToAsm cls)
  $ runCodeGen (Map.fromList <$> compileProgram prog) cls

interpQuad :: ClassMap -> Program -> IO ()
interpQuad cls prog =
  let (localCounts, quads) = runCodeGen (compileProgram prog) $! cls
      localCountMap = Map.fromList localCounts
  in qInter cls localCountMap quads


execCodeGen :: CodeGen a -> ClassMap -> [Quad]
execCodeGen cg = snd . runCodeGen cg

runCodeGen :: CodeGen a -> ClassMap -> (a, [Quad])
runCodeGen cg m =
  evalState (runReaderT (runWriterT cg) (m, noBaseClass, Map.empty, Map.empty)) 0

type LocalCount = (Label, Integer)

compileProgram :: Program -> CodeGen [LocalCount]
compileProgram (Program topDefs) = concatMapM compileTopDef topDefs

compileTopDef :: TopDef -> CodeGen [LocalCount]
compileTopDef (StrDef n _ mems) = enterClass n $ concatMapM compileMember mems
compileTopDef (TopFnDef fndef) = compileFunction fndef

compileMember :: MemberDecl -> CodeGen [LocalCount]
compileMember (FieldDecl _ _) = return []
compileMember (MethDecl fndef) = compileFunction fndef

isInClass :: CodeGen Bool
isInClass = liftM (/= noBaseClass) getCurrentClass

variablesToClasses :: [(Ident, Type)] -> [(Ident, Ident)]
variablesToClasses =
  map (second $ \(Struct name) -> name)
  . filter (isClass . snd)

compileFunction :: FnDef -> CodeGen [LocalCount]
compileFunction (FnDef t name args (Block stmts)) = do
  isMethod <- isInClass
  clsName <- asks snd4
  fields <- getAllFields clsName
  let argNames = map getArgName args
      argNames' = if isMethod then Ident "self" : argNames else argNames
      args' = zip argNames' $ map (Loc LArg) [1..]
      stmts' = runLocalMangler stmts
      localDecls = concatMap getIdentDeclsDeep stmts'
      localNames = map fst localDecls
      locals = zip localNames $ map (Loc LLocal) [1..]
      fieldNames = map fst fields
      fields' = zip fieldNames $ map (Loc LObj) [1..]
      env = Map.fromList $ args' ++ locals ++ fields'
      objClasses = Map.fromList $
                     variablesToClasses localDecls
                     ++ variablesToClasses fields
                     ++ variablesToClasses (map argToType args)
                     ++ if isMethod then [(Ident "self", clsName)] else []
  mangled <- mangle name
  local (third4 (const env) . fourth (const objClasses)) $ censor ((:[]) . QFun mangled) $ do
    compileStmts stmts'
    when (t == Latte.Void) ret
  return [(mangled, toInteger $ length localNames)]


mangle :: Ident -> CodeGen Ident
mangle name = do
  clsName <- getCurrentClass
  return $ if clsName == noClass
    then name
    else mangler clsName name

compileStmts :: [Stmt] -> CodeGen ()
compileStmts = mapM_ compileStmt

compileStmt :: Stmt -> CodeGen ()
compileStmt Empty = return ()
compileStmt (BStmt (Block stmts)) = compileStmts stmts
compileStmt (Decl t items) = mapM_ (flip compileItem t) items
compileStmt (Ass name expr) = do
  val <- compileExpr expr
  loc <- getLoc name
  tell [QCopy (VLoc loc) val]
compileStmt (MemAss objName memName expr) = do
  val <- compileExpr expr
  objVal <- getVal objName
  clsName <- getClassOfVar objName
  tell [QCopy (VMember objVal clsName memName) val]
compileStmt (Incr name) = do
  v <- getVal name
  tell [QBin v v (OAdd Plus) (VImm 1)]
compileStmt (Decr name) = do
  v <- getVal name
  tell [QBin v v (OAdd Minus) (VImm 1)]
compileStmt (Ret e) = do
  v <- compileExpr e
  tell [QRet v]
compileStmt VRet = tell [QVRet]
compileStmt (Cond cond stmt) = do
  l1 <- freshLabel
  l2 <- freshLabel
  compileCond cond l1 l2
  label l1
  compileStmt stmt
  label l2
compileStmt (CondElse cond s1 s2) = do
  l1 <- freshLabel
  l2 <- freshLabel
  l3 <- freshLabel
  compileCond cond l1 l2
  label l1
  compileStmt s1
  goto l3
  label l2
  compileStmt s2
  label l3
compileStmt (While cond s) = do
  loop <- freshLabel
  checkCond <- freshLabel
  endLoop <- freshLabel
  goto checkCond
  label loop
  compileStmt s
  label checkCond
  compileCond cond loop endLoop
  label endLoop
compileStmt (StExp expr) = void $ compileExpr expr

freshLabel :: CodeGen Ident
freshLabel = do
  i <- get
  put (i + 1)
  return (Ident ("Label" ++ show i))

freshReg :: CodeGen Val
freshReg = do
  i <- get
  put (i + 1)
  return (VReg i)

label :: Ident -> CodeGen ()
label name = tell [QLabel name]

goto :: Ident -> CodeGen ()
goto l = tell [QGoto l]

ret :: CodeGen ()
ret = tell [QVRet]

getVal :: Ident -> CodeGen Val
getVal = fmap VLoc . getLoc

getLoc :: Ident -> CodeGen Loc
getLoc ident = do
  locMap <- getLocMap
  return $ locMap ! ident

compileExpr :: Expr -> CodeGen Val
compileExpr (EVar v) = getVal v
compileExpr (ELitInt i) = return $ VImm i
compileExpr ELitTrue = return true
compileExpr ELitFalse = return false
compileExpr ENull = return VNullptr
compileExpr (ENew name) = do
  r <- freshReg
  tell [QNew r name]
  return r
compileExpr (EApp fname args) = do
  vs <- mapM compileExpr args
  isMethod <- isInClass
  isMethod' <-
    if isMethod
      then getCurrentClass >>= isAMethod fname
      else return isMethod
  let vs' = if isMethod' then VLoc (Loc LArg 1):vs else vs
  mindex <-
    if isMethod'
      then fmap Just $ getCurrentClass >>= getMethodIndex fname
      else return Nothing
  r <- freshReg
  tell [QCall r fname mindex vs']
  return r
compileExpr (EString s) = return (VStr s)
compileExpr (ECast _) = return VNullptr
compileExpr (EAcc objName memName) =
  VMember <$> getVal objName <*> getClassOfVar objName <*> pure memName
compileExpr (EMeth objName methName args) = do
  v <- getVal objName
  vs <- mapM compileExpr args
  r <- freshReg
  name <- getClassOfVar objName
  index <- getMethodIndex methName name
  tell [QCall r (markMethod methName) (Just index) (v:vs)]
  return r
compileExpr (Neg e) = compileUnOp ONeg e
compileExpr (Not e) = compileUnOp ONot e
compileExpr (EMul e1 op e2) = compileBinOp e1 (OMul op) e2
compileExpr (EAdd e1 op e2) = compileBinOp e1 (OAdd op) e2 -- TODO: string concatenation
compileExpr (ERel e1 op e2) = compileBinOp e1 (ORel op) e2
compileExpr (EAnd e1 e2) = do
  v <- compileExpr e1
  r <- freshReg
  l <- freshLabel
  end <- freshLabel
  jumpIf v EQU false l
  v2 <- compileExpr e2
  tell [QCopy r v2]
  goto end
  label l
  tell [QCopy r false]
  label end
  return r
compileExpr (EOr e1 e2) = do
  v <- compileExpr e1
  r <- freshReg
  l <- freshLabel
  end <- freshLabel
  jumpIf v EQU true l
  v2 <- compileExpr e2
  tell [QCopy r v2]
  goto end
  label l
  tell [QCopy r true]
  label end
  return r

compileUnOp :: UnOp -> Expr -> CodeGen Val
compileUnOp op e = do
  v <- compileExpr e
  r <- freshReg
  tell [QUn r op v]
  return r
compileBinOp :: Expr -> Op -> Expr -> CodeGen Val
compileBinOp e1 op e2 = do
  v1 <- compileExpr e1
  v2 <- compileExpr e2
  r <- freshReg
  tell [QBin r v1 op v2]
  return r

compileCond :: Expr -> Label -> Label -> CodeGen ()
compileCond e ifTrue ifFalse = go e
  where
    go ELitTrue = goto ifTrue
    go ELitFalse = goto ifFalse
    go (EVar v) = do
      v' <- getVal v
      jumpIf v' EQU true ifTrue
      goto ifFalse
    go (Not e') = compileCond e' ifFalse ifTrue
    go (EAnd e1 e2) = do
      l <- freshLabel
      compileCond e1 l ifFalse
      label l
      compileCond e2 ifTrue ifFalse
    go (EOr e1 e2) = do
      l <- freshLabel
      compileCond e1 ifTrue l
      label l
      compileCond e2 ifTrue ifFalse
    go (ERel e1 op e2) = do
      v1 <- compileExpr e1
      v2 <- compileExpr e2
      jumpIf v1 op v2 ifTrue
      goto ifFalse
    go e' = do
      v <- compileExpr e'
      jumpIf v EQU true ifTrue
      goto ifFalse

true :: Val
true = VImm 1

false :: Val
false = VImm 0

jumpIf :: Val -> RelOp -> Val -> Label -> CodeGen ()
jumpIf v1 op v2 l = tell [QCond v1 op v2 l]

compileItem :: Item -> Type -> CodeGen ()
compileItem (NoInit name) Str = do
  v <- getVal name
  tell [v $= VStr ""]
compileItem (NoInit name) _ = do
  v <- getVal name
  tell [v $= VImm 0]
compileItem (Init name e) _ = compileStmt $ Ass name e
