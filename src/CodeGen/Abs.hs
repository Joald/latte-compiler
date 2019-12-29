module CodeGen.Abs where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Map (Map)

import Types.Abs

import BNFC.AbsLatte

data Quad
  = QBin Val Val Op Val
  | QUn Val UnOp Val
  | QCopy Val Val
  | QGoto Label
  | QLabel Label
  | QCond Val RelOp Val Label
  | QCall Val Label [Val]
  | QRet Val
  | QVRet
  | QMemberAss Val Label Val -- object . field = value
  | QMethCall Val Val Label [Val] -- rv, obj, fname, args
  | QNew Val Ident -- val = new Name
  deriving (Show, Eq, Ord, Read)

showQuad :: Quad -> String
showQuad (QLabel (Ident name)) = name ++ ":"
showQuad q = "  " ++ case q of
  QBin v1 v2 op v3 ->
    showVal v1 ++ " = " ++ showVal v2 ++ " " ++ showOp op ++ " " ++ showVal v3
  QUn v1 op v2 -> showVal v1 ++ " = " ++ showUnOp op ++ showVal v2
  QCopy v1 v2 -> showVal v1 ++ " = " ++ showVal v2
  QGoto (Ident name) -> "goto " ++ name
  QCond v1 op v2 (Ident name) ->
    "if " ++ showVal v1 ++ " " ++ showRelOp op ++ " " ++ showVal v2
          ++ " goto " ++ name
  QCall v (Ident l) vs ->
    "call " ++ showVal v ++ " = " ++ l ++
    "(" ++ intercalate ", " (map showVal vs) ++ ")"
  QRet v -> "ret " ++ showVal v
  QVRet -> "ret"
  QMemberAss vo (Ident fld) v -> showVal vo ++ " . " ++ fld ++ " = " ++ showVal v
  QMethCall vret vobj (Ident methName) vs ->
    showVal vret ++ " = " ++ showVal vobj ++ " . " ++ methName
              ++ "(" ++ intercalate ", " (map showVal vs) ++ ")"
  QNew v (Ident clsName) -> showVal v ++ " = new " ++ clsName
  _ -> error "showQuad: can never happen"


infix 2 $=
($=) :: Val -> Val -> Quad
($=) = QCopy

data UnOp = ONeg | ONot
  deriving (Show, Eq, Ord, Read)

showUnOp :: UnOp -> String
showUnOp ONeg = "-"
showUnOp ONot = "!"

type Label = Ident

data Val
  = VReg Integer
  | VStr String
  | VLoc Loc
  | VImm Integer
  | VNullptr
  | VMember Val Label
  deriving (Show, Eq, Ord, Read)

showVal :: Val -> String
showVal (VReg i) = "reg " ++ show i
showVal (VStr s) = show s
showVal (VLoc l) = showLoc l
showVal (VImm i) = "$" ++ show i
showVal VNullptr = "nullptr"
showVal (VMember v (Ident l)) = show v ++ "." ++ l

data Loc = Loc Region Integer
  deriving (Show, Eq, Ord, Read)

showLoc :: Loc -> String
showLoc (Loc LArg i) = "args[" ++ show i ++ "]"
showLoc (Loc LLocal i) = "local[" ++ show i ++ "]"

data Region = LArg | LLocal
  deriving (Show, Eq, Ord, Read)
data Op = OAdd AddOp | OMul MulOp | OAnd | OOr | ORel RelOp
  deriving (Show, Eq, Ord, Read)

showOp :: Op -> String
showOp (OAdd Plus) = "+"
showOp (OAdd Minus) = "-"
showOp (OMul Times) = "*"
showOp (OMul Div) = "/"
showOp (OMul Mod) = "%"
showOp OAnd = "&&"
showOp OOr = "||"
showOp (ORel op) = showRelOp op
showRelOp :: RelOp -> String
showRelOp op =
  case op of
    LTH -> "<"
    LE -> "<="
    GTH -> ">"
    GE -> ">="
    EQU -> "=="
    NE -> "!="
data Asm
  = Aret
  | Afn Int [Asm] -- Int is the required stack space
  deriving Show

data AsmReg = EAX | ECX | EDX | EBP
  deriving (Show, Eq, Ord, Read)

showAsm :: Asm -> String
showAsm = show



type LocMap = Map Ident Loc

type Env = (ClassMap, Ident, LocMap)
type St = Integer

type CodeGen = WriterT [Quad] (ReaderT Env (State St))
