{-# LANGUAGE BlockArguments #-}
module CodeGen.Asm (quadToAsm) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe

import BNFC.AbsLatte
import Utils
import Types.Abs
import CodeGen.Abs
import CodeGen.Utils

quadToAsm :: ClassMap -> LocalCounts -> [Quad] -> [Asm]
quadToAsm clsMap localCounts qs =
  let fnNames = Map.keys localCounts
  --    clsMap' = polymorphisation clsMap
  in runQTA (qta qs) clsMap localCounts

runQTA :: QTA a -> ClassMap -> LocalCounts -> [Asm]
runQTA m clsMap localCounts =
  evalState (runReaderT (execWriterT m) (clsMap, localCounts))
            (constMapper $ error . ("no param for value "++) . showVal, 0)

constMapper :: (Val -> Param) -> Val -> Param
constMapper f v =
  case v of
    VImm i -> PImm i
    VNullptr -> PImm 0
    VLoc (Loc LArg i) -> argAddr i
    _ -> f v

mapMapper :: AsmMap -> (Val -> Param) -> Val -> Param
mapMapper map f v = fromMaybe (f v) (Map.lookup v map)

insertMapper :: AsmMap -> QTA ()
insertMapper = modify . first . mapMapper

{-type QFunMap = Map Ident [Quad]

divideQs :: [Ident] -> [Quad] -> QFunMap
divideQs [] [] = Map.empty
divideQs ids qs = Map.fromList $ go ids qs
  where
    go :: [Ident] -> [Quad] -> [(Ident, [Quad])]
    go _ [] = []
    go ids (QLabel l:qs) =
      let (lres, rest) = go' l ids qs
      in (l, lres) : rest
    go _ _ = error "code must start with a label"
    go' :: Ident -> [Ident] -> [Quad] -> ([Quad], [(Ident, [Quad])])
    go' curId ids qall@(i@(QLabel l):qs) =
      if l `elem` ids then ([], go ids qall) else
        first (i:) $ go' curId ids qs
    go' _ _ [] = ([], [])
    go' curId ids (q:qs) = first (q:) $ go' curId ids qs

polymorphisation :: ClassMap -> ClassMap
polymorphisation =
  runReader $ fmap Map.fromList $ asks (map fst . Map.toList) >>= mapM go
  where
    go clsName = do
      flds <- getAllFields clsName
      meths <- getAllMethods clsName
      let meths' = map snd meths
      ts <- mapM (getMemberType clsName) meths'
      let mems' = Map.fromList $ flds ++ zip meths' ts
      return (clsName, Class clsName noBaseClass mems')
-}
custom :: String -> QTA ()
custom s = tell [ACustom s]

intoSection :: Section -> QTA a -> QTA a
intoSection s = censor $ (:[]) . ASection s

qta :: [Quad] -> QTA ()
qta qs = do
  custom "DEFAULT REL"
  custom "global main"
  mapM_ (custom . ("extern " ++) . unIdent) builtins
  custom $ "extern " ++ unIdent calloc
  strs <- intoSection SRodata $ generateConstants qs
  insertMapper strs
  ctors <- snd <$> listen' createCtors
  code <- snd <$> listen' (createFns qs)
--  intoSection SBss $ generateConstants constants
  section SText (code ++ ctors)

generateConstants :: [Quad] -> QTA AsmMap
generateConstants qs = do
  custom "  zero: dd 0"
  generateVTables
  generateStrings qs

generateVTables :: QTA ()
generateVTables = do
  cmap <- getClassMap
  mapM_ createVTable $ Map.elems cmap

generateStrings :: [Quad] -> QTA AsmMap
generateStrings qs = do
  let strs = gatherStrings qs
  labels <- mapM constStr strs
  return $ Map.fromList $ zipWith (\s l -> (VStr s, PLabel l)) strs labels

getNext :: QTA Integer
getNext = do
  i <- gets snd
  modify $ second $ const (i + 1)
  return i

constStr :: String -> QTA Label
constStr s = do
  i <- getNext
  let str = "str" ++ show i
  tell [AConstStr str s]
  return $ Ident str

gatherStrings :: [Quad] -> [String]
gatherStrings = concatMap $ mapQuad go
  where
    go :: Val -> [String]
    go (VStr s) = [s]
    go (VMember v _ _) = go v
    go _ = []

section :: Section -> [Asm] -> QTA ()
section sec asm = tell [ASection sec asm]

createCtors :: QTA ()
createCtors = do
  cmap <- getClassMap
  let classes = Map.elems cmap
  mapM_ createCtor classes

label :: Label -> QTA ()
label l = tell [ALabel l]

ctor :: Ident -> Ident
ctor = mapIdent $ (ctorMarker ++) . (++ ctorMarker)

vTable :: Ident -> Ident
vTable = mapIdent $ (vTableMarker ++) . (++ vTableMarker)

ctorMarker :: String
ctorMarker = "__ctor__"

vTableMarker :: String
vTableMarker = "__vTable__"

leave :: QTA ()
leave = tell [Aleave]

enter :: QTA ()
enter = tell [Aenter]

push :: Param -> QTA ()
push param = tell [AUn PUSH param]

addEsp :: Integer -> QTA ()
addEsp i = tell [ABin ADD (PReg ESP) (PImm i)]

subEsp :: Integer -> QTA ()
subEsp i = tell [ABin SUB (PReg ESP) (PImm i)]

argAddr :: Integer -> Param
argAddr i = PAddr EBP Nothing Nothing (Just $ -4 * i)

addrExact :: Reg -> Param
addrExact reg = PAddr reg Nothing Nothing Nothing

addrOffset :: Reg -> Integer -> Param
addrOffset reg offset = PAddr reg Nothing Nothing (Just $ 4 * offset)

addrIndex :: Reg -> Reg -> Param
addrIndex reg index = PAddr reg (Just 4) (Just index) Nothing

localAddr :: Integer -> Param
localAddr = addrOffset EBP

assignLocal :: Integer -> Param -> QTA ()
assignLocal li p = mov (localAddr li) p

jmp :: Label -> QTA ()
jmp l = tell [AUn JMP (PLabel l)]

call :: Label -> QTA ()
call l = tell [AUn CALL (PLabel l)]

eax :: Param
eax = PReg EAX

ecx :: Param
ecx = PReg ECX

edx :: Param
edx = PReg EDX

calloc :: Label
calloc = Ident "__internal__calloc"

zero :: Label
zero = Ident "__internal__zero"

mov :: Param -> Param -> QTA ()
mov p1 p2 = tell [ABin MOV p1 p2]

xor :: Param -> Param -> QTA ()
xor p1 p2 = tell [ABin XOR p1 p2]

createCtor :: Class -> QTA ()
createCtor cls@(Class name _ _) = do
  label $ ctor name
  let flds = getFields cls
      classSize = toInteger $ length flds + 1 -- every class gets a virtual table
  enter
  subEsp 8
  push $ PImm 4
  push $ PImm classSize
  subEsp 8
  call calloc
  addEsp 16
  let vt = vTable name
  mov (addrExact EAX) (PLabel vt)
  ts <- mapM (getMemberType name) flds
  mov edx eax
  zipWithM_ initializeField ts $ map (addrOffset EDX) [4,8..]
  leave
  iret $ PReg EDX

createVTable :: Class -> QTA ()
createVTable (Class name _ _) = do
  fns <- getAllMethods name
  let names = map fst fns
  tell [Aequ (vTable name) names]

ret :: QTA ()
ret = tell [Aret]

iret :: Param -> QTA ()
iret p = mov eax p >> ret

initializeField :: Type -> Param -> QTA ()
initializeField Str p = mov p (PLabel zero)
initializeField _ _ = return ()

createFns :: [Quad] -> QTA ()
createFns = mapM_ createFn

createFn :: Quad -> QTA ()
createFn (QFun name qs) = do
  label name
  enter
  lc <- getLocalCount name
  let regs = regsUsed qs
      stackSize = 4 * (lc + toInteger (length regs) + 1)
      realStackSize = if stackSize `mod` 16 == 8 then stackSize else stackSize + 8
  subEsp realStackSize
  let locMap = Map.fromList $
           map (\i -> (VLoc (Loc LLocal i), localAddr i)) [1..lc]
        ++ zipWith (\r i -> (VReg r, localAddr (lc + i))) regs [1..]
  insertMapper locMap -- TODO: make sure this doesn't need reversing
  mapM_ cq qs
createFn qs = error $ "malformed function:\n" ++ showQuad qs

getLocalCount :: Ident -> QTA Integer
getLocalCount name = askMap name snd

regsUsed :: [Quad] -> [Integer]
regsUsed = nub . concatMap (mapQuad go)
  where
    go :: Val -> [Integer]
    go (VReg i) = [i]
    go (VMember v _ _) = go v
    go _ = []

-- parameter returned is valid as a source only during next gen operation
valToParam :: Val -> QTA Param
valToParam (VLoc (Loc LObj i)) = do
  mov ecx (argAddr 1)
  mov ecx (addrOffset ECX i)
  return ecx
valToParam (VMember v cls l) = do
  p <- valToParam v
  mov ecx p
  i <- getFieldOffset cls l
  mov ecx (addrOffset ECX i)
  return ecx
valToParam v = gets fst <*> pure v

binAsm :: BinAsm -> Param -> Param -> QTA ()
binAsm b p1 p2 = tell [ABin b p1 p2]

unAsm :: UnAsm -> Param -> QTA ()
unAsm b p = tell [AUn b p]

opToBinAsm :: AddOp -> BinAsm
opToBinAsm Plus = ADD
opToBinAsm Minus = SUB

mulOpToUnAsm :: MulOp -> UnAsm
mulOpToUnAsm Times = IMUL
mulOpToUnAsm _ = IDIV

doOp :: Op -> Param -> QTA Param
doOp op pr =
  case op of
    OAdd addOp -> do
      binAsm (opToBinAsm addOp) eax pr
      return eax
    OMul mulOp -> do
      unAsm (mulOpToUnAsm mulOp) pr
      return if mulOp == Mod then edx else eax
    OAnd -> do
      binAsm AND eax pr
      return eax
    OOr -> do
      binAsm OR eax pr
      return eax
    ORel op -> do
      l <- freshLabel
      binAsm CMP eax pr
      mov eax (PImm 1) -- assume it's true and correct if not
      unAsm (relToJmp op) (PLabel l)
      xor eax eax
      label l
      return eax

relToJmp :: RelOp -> UnAsm
relToJmp op =
  case op of
    LTH -> JL
    LE -> JLE
    GTH -> JG
    GE -> JGE
    EQU -> JE
    NE -> JNE

freshLabel :: QTA Label
freshLabel = do
  i <- getNext
  return $ Ident $ "asmLabel" ++ show i

doUnOp :: UnOp -> QTA Param
doUnOp op =
  case op of
    ONot -> xor eax (PImm 1) >> return eax
    ONeg -> unAsm NEG eax >> return eax

-- cq (compileQuad) assumes that registers have garbage values in the beginning
cq :: Quad -> QTA ()
cq (QBin vres vl op vr) = do
  xor edx edx
  pl <- valToParam vl
  mov eax pl
  pr <- valToParam vr
  opRes <- doOp op pr
  pres <- valToParam vres
  mov pres opRes
cq (QUn vres op v) = do
  p <- valToParam v
  mov eax p
  opRes <- doUnOp op
  pres <- valToParam vres
  mov pres opRes
cq (QCopy vl vr) = do
  pr <- valToParam vr
  mov eax pr
  pl <- valToParam vl
  mov pl eax
cq (QGoto l) = jmp l
cq (QLabel l) = label l
cq (QCond vl op vr l) = do
  pl <- valToParam vl
  mov eax pl
  pr <- valToParam vr
  binAsm CMP eax pr
  unAsm (relToJmp op) (PLabel l)
cq (QCall vres name mindex args) = do
  let padding = toInteger $ 16 - 4 * (length args `mod` 4)
  unless (padding `mod` 16 == 0) $ subEsp padding
  mapM_ pushArg $ reverse args
  binAsm ADD (PReg ESP) (PImm padding)
  maybe (call name) (callVirtual (head args)) mindex
  pres <- valToParam vres
  mov pres eax
cq (QRet v) = do
  p <- valToParam v
  mov eax p
  leave
  ret
cq QVRet = leave >> ret
cq (QNew v name) = do
  call $ ctor name
  p <- valToParam v
  mov p eax

pushArg :: Val -> QTA ()
pushArg v = valToParam v >>= push

callVirtual :: Val -> Integer -> QTA ()
callVirtual v i = do
  p <- valToParam v
  mov eax p
  mov eax (addrExact EAX)
  mov eax (addrOffset EAX i)
  unAsm CALL (addrExact EAX)
