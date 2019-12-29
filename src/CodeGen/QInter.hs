module CodeGen.QInter where

import Control.Monad.Reader
import Control.Monad.State

import Foreign.Marshal.Utils (fromBool)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import BNFC.AbsLatte hiding (Int, Bool, Void)
import qualified BNFC.AbsLatte as Latte
import Data.List
import Data.Maybe
import Data.Bits
import System.Exit (exitFailure)
import CodeGen.Abs
import CodeGen.Utils (getFields, mangler)
import Types.Abs
import Utils

type QCode = Map Integer Quad
type QLabels = Map Label Integer
type QLocalCounts = Map Label Integer
type QHeap = Map Integer V
type QRegs = Map Integer V
type QStack = Map Loc V
type QSt = (QHeap, QRegs, QStack)

type Mapper a = a -> a

modifyHeap :: Mapper QHeap -> QInter ()
modifyHeap f = modify $ first3 f

getHeap :: QInter QHeap
getHeap = gets fst3

getHeapVal :: Integer -> QInter V
getHeapVal i = getMap i fst3

modifyHeapVal :: Integer -> V -> QInter ()
modifyHeapVal i v = modifyHeap $ Map.insert i v

modifyRegs :: Mapper QRegs -> QInter ()
modifyRegs f = modify $ second3 f

modifyReg :: Integer -> V -> QInter ()
modifyReg k v = modifyRegs $ Map.insert k v

getRegVal :: Integer -> QInter V
getRegVal i = gets $ (! i) . snd3

type QEnv = (QCode, QLabels, ClassMap, QLocalCounts)

getInstrAt :: Integer -> QInter Quad
getInstrAt i = askMap i fst4

getLabelPos :: Label -> QInter Integer
getLabelPos l = askMap l snd4

getClass :: Ident -> QInter Class
getClass name = askMap name thd4

getStack :: QInter QStack
getStack = gets thd3

putStack :: QStack -> QInter ()
putStack = modify . third . const

newStackFrame :: QStack -> QInter a -> QInter a
newStackFrame s m = do
  (old1, old2, old3) <- get
  put (old1, old2, s)
  ret <- m
  putStack old3
  modify . second3 $ const old2
  return ret

modifyStack :: Loc -> V -> QInter ()
modifyStack l v = modify $ third $ Map.insert l v

getStackVal :: Loc -> QInter V
getStackVal l = getMap l thd3

getLocalCounts :: QInter QLocalCounts
getLocalCounts = asks frh4

data V = VInt Integer | VString String | VObj Label [V] | VVoid | VAddr Integer
  deriving (Show, Eq, Ord, Read)

type QInter = StateT QSt (ReaderT QEnv IO)

getLabel :: (Integer, Quad) -> [(Label, Integer)]
getLabel (i, QLabel l) = [(l, i)]
getLabel _ = []

qInter :: ClassMap -> QLocalCounts -> [Quad] -> IO ()
qInter cls locals qs = do
  let qmap = Map.fromList $ zip [1..] qs
      qLabels = Map.fromList $ concatMap getLabel $ Map.toList qmap
      initEnv = (qmap, qLabels, cls, locals)
      initSt = (Map.empty, Map.empty, Map.empty)
  putErrLn $ "Compiled Quad code: \n" ++ unlines (map showQuad qs)
  putErrLn "Interpreting program..."
  VInt res <- runReaderT (evalStateT (call mainID []) initSt) initEnv
  putErrLn $ "Process returned " ++ show res

callBuiltin :: Ident -> [V] -> QInter V
callBuiltin (Ident name) arg =
  case (name, arg) of
    ("printInt", [VInt i])       -> liftIO (print i) >> return VVoid
    ("printString", [VString s]) -> liftIO (putStrLn s) >> return VVoid
    ("error", [])                -> liftIO (putStrLn "error") >> liftIO exitFailure
    ("readInt", [])              -> VInt <$> liftIO readLn
    ("readString", [])           -> VString <$> liftIO getLine
    _ -> error "xd"

call :: Label -> [V] -> QInter V
call name args =
  if name `elem` builtins then callBuiltin name args else do
    sf <- createStackFrame name args
    p <- getLabelPos name
    liftIO $ putErrLn $ "Calling fn " ++ unIdent name ++ " with " ++ show args
    newStackFrame sf $ run (p + 1)

createStackFrame :: Label -> [V] -> QInter QStack
createStackFrame name args = do
  locals <- getLocalCounts
  return $ Map.fromList $
       zip (map (Loc LArg)   [1..]) args
    ++ zip (map (Loc LLocal) [1..]) (map VInt [1..locals ! name])

deb :: String -> QInter ()
deb = liftIO . putErrLn

run :: Integer -> QInter V
run i = do
  instr <- getInstrAt i
  liftIO $ putErrLn $ "Executing instruction " ++ showQuad instr
  case instr of
    QRet val -> do
      rv <- eVal val
      deb $ "Returning " ++ show rv
      return rv
    QVRet    -> deb "returning from function" >> return VVoid
    _        -> evalInstr instr i >>= run

doBinOp :: V -> Op -> V -> V
doBinOp (VInt l) op (VInt r) = VInt $ getBinOp op l r
doBinOp (VString s1) (OAdd Plus) (VString s2) = VString $ s1 ++ s2
doBinOp v1 (ORel EQU) v2 = VInt $ fromBool (v1 == v2)
doBinOp v1 (ORel NE) v2 = VInt $ fromBool (v1 /= v2)
doBinOp _ _ _ = error "doBinOp: can never happen"

doUnOp :: UnOp -> V -> V
doUnOp ONeg (VInt i) = VInt (-i)
doUnOp ONot (VInt i) = VInt $ complementBit i 0
doUnOp _ _ = error "doUnOp: can never happen"

assign :: Val -> V -> QInter ()
assign (VReg i) v = modifyReg i v
assign (VLoc l) v = modifyStack l v
assign (VMember val label) v = do
  VAddr addr <- eVal val
  VObj clsName fields <- getHeapVal addr
  (_, i) <- extractFieldOffset val label
  let newFields = replaceIth v i fields
      newObj = VObj clsName newFields
  modifyHeapVal addr newObj
assign _ _ = error "assign: can never happen"

replaceIth :: a -> Integer -> [a] -> [a]
replaceIth v i l =
  let i' = fromInteger i
  in take i' l ++ v : drop (i' + 1) l

evalInstr :: Quad -> Integer -> QInter Integer
evalInstr (QBin vres vl op vr) i = do
  vres' <- doBinOp <$> eVal vl <*> pure op <*> eVal vr
  assign vres vres'
  return $ i + 1
evalInstr (QUn vres unOp v) i = do
  vres' <- doUnOp unOp <$> eVal v
  assign vres vres'
  return $ i + 1
evalInstr (QCopy vl vr) i = do
  vr' <- eVal vr
  assign vl vr'
  return $ i + 1
evalInstr (QGoto l) _ = getLabelPos l
evalInstr (QLabel _) i = return $ i + 1
evalInstr (QCond vl relop vr l) i = do
  p <- getLabelPos l
  VInt cond <- doBinOp <$> eVal vl <*> pure (ORel relop) <*> eVal vr
  return $ if testBit cond 0 then p else i + 1
evalInstr (QCall vres name args) i = do
  vs <- mapM eVal args
  res <- call name vs
  assign vres res
  return $ i + 1
evalInstr (QMemberAss obj fld val) i =
  eVal val >>= assign (VMember obj fld) >> return (i + 1)
{-evalInstr (QMemberAccess vres obj fld) i =
  eVal (VMember obj fld) >>= assign vres >> return (i + 1)-}
evalInstr (QMethCall vres obj mth args) i = do
  vs <- mapM eVal args
  VAddr addr <- eVal obj
  v@(VObj clsName _) <- getHeapVal addr
  let mangled = mangler clsName mth
  res <- call mangled (v:vs)
  assign vres res
  return $ i + 1
evalInstr (QNew vres clsName) i = do
  heap <- getHeap
  flds <- getAllFields clsName
  let n = 1 + fromMaybe 0 (fst <$> Map.lookupMax heap)
      vs = map defaultInit flds
  modifyHeapVal n $ VObj clsName vs
  assign vres (VAddr n)
  return $ i + 1

getAllFields :: Ident -> QInter [Type]
getAllFields name = if name == noBaseClass then return [] else do
  cls@(Class _ base mems) <- getClass name
  let flds = map (mems!) (getFields cls)
  rest <- getAllFields base
  return $ flds ++ rest

defaultInit :: Type -> V
defaultInit Latte.Int = VInt 0
defaultInit Latte.Bool = VInt 0
defaultInit Str = VString ""
defaultInit (Struct _) = VAddr 0
defaultInit _ = error "defaultInit: can never happen"



eVal :: Val -> QInter V
eVal (VReg i) = getRegVal i
eVal (VStr str) = return $ VString str
eVal (VLoc l) = getStackVal l
eVal (VImm i) = return $ VInt i
eVal VNullptr = return $ VAddr 0
eVal (VMember v memName) =
  uncurry (!!) . (second fromInteger) <$> extractFieldOffset v memName

extractFieldOffset :: Val -> Label -> QInter ([V], Integer)
extractFieldOffset v memName = do
  VAddr v' <- eVal v
  VObj clsName fields <- getHeapVal v'
  i <- getFieldOffset clsName memName
  return (fields, i)

getFieldOffset :: Ident -> Ident -> QInter Integer
getFieldOffset clsName fldName = do
  cls <- getClass clsName -- assumes this is never called with clsName == Ident ""
  let mi = fldName `elemIndex` getFields cls
  maybe
    (getFieldOffset (classBase cls) fldName)
    (return . toInteger)
    mi
