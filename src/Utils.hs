{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Prelude hiding (id)
import System.IO
import Foreign.Marshal.Utils (fromBool)
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.Bits
import Data.Map (Map, (!))
import qualified Data.Map as Map

import CodeGen.Abs
import BNFC.AbsLatte hiding (Int, Bool, Void)
import Types.Abs

putErr :: String -> IO ()
putErr = hPutStr stderr

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr


first :: (a -> c) -> (a, b) -> (c, b)
first f ~(a, b) = (f a, b)

second :: (b -> c) -> (a, b) -> (a, c)
second f ~(a, b) = (a, f b)

first3 :: (a -> c) -> (a, b, d) -> (c, b, d)
first3 f ~(a, b, c) = (f a, b, c)

second3 :: (b -> c) -> (a, b, d) -> (a, c, d)
second3 f ~(a, b, c) = (a, f b, c)

third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f ~(a, b, c) = (a, b, f c)

fourth4 :: (d -> f) -> (a, b, c, d) -> (a, b, c, f)
fourth4 f ~(a, b, c, d) = (a, b, c, f d)

fourth5 :: (d -> f) -> (a, b, c, d, e) -> (a, b, c, f, e)
fourth5 f ~(a, b, c, d, e) = (a, b, c, f d, e)

fst3 :: (a, b, c) -> a
fst3 ~(x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 ~(_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 ~(_, _, x) = x

fst4 :: (a, b, c, d) -> a
fst4 ~(x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 ~(_, x, _, _) = x

thd4 :: (a, b, c, d) -> c
thd4 ~(_, _, x, _) = x

frh4 :: (a, b, c, d) -> d
frh4 ~(_, _, _, x) = x

fst5 :: (a, b, c, d, e) -> a
fst5 ~(x, _, _, _, _) = x

snd5 :: (a, b, c, d, e) -> b
snd5 ~(_, x, _, _, _) = x

thd5 :: (a, b, c, d, e) -> c
thd5 ~(_, _, x, _, _) = x

frh5 :: (a, b, c, d, e) -> d
frh5 ~(_, _, _, x, _) = x

fth5 :: (a, b, c, d, e) -> e
fth5 ~(_, _, _, _, x) = x

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond ifTrue ifFalse = do
  res <- cond
  if res then ifTrue else ifFalse

(!-!) :: Maybe a -> Maybe a -> Maybe a
l !-! r = if isJust l then l else r

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (concat <$>) . mapM f

inhToIdent :: Inheritance -> Ident
inhToIdent NoInherit = noBaseClass
inhToIdent (Extends id) = id

funDefType :: FnDef -> Type
funDefType (FnDef ty _ args _) = Fun ty $ map (\(Arg t _) -> t) args

unIdent :: Ident -> String
unIdent (Ident id) = id

getIdentDecls :: Stmt -> [Ident]
getIdentDecls (Decl _ items) = map itemName items
getIdentDecls  _ = []

getMember :: MemberDecl -> (Ident, Type)
getMember (FieldDecl t id) = (id, t)
getMember (MethDecl fn@(FnDef _ id _ _)) = (id, funDefType fn)


noBaseClass :: Ident
noBaseClass = Ident ""

isTrivially :: Bool -> Expr -> Bool
isTrivially True ELitTrue = True
isTrivially False ELitFalse = True
isTrivially True (Neg e) = isTrivially False e
isTrivially False (Neg e) = isTrivially True e
isTrivially True (EAnd e1 e2) = isTrivially True e1 && isTrivially True e2
isTrivially False (EAnd e1 e2) = isTrivially False e1 || isTrivially False e2
isTrivially True (EOr e1 e2) = isTrivially True e1 || isTrivially True e2
isTrivially False (EOr e1 e2) = isTrivially False e1 && isTrivially False e2
isTrivially True (ERel e1 EQU e2) = hasNoSideEffects e1 && hasNoSideEffects e2 && e1 == e2
isTrivially False (ERel e1 EQU e2) = hasNoSideEffects e1 && hasNoSideEffects e2 && e1 /= e2
isTrivially True (ERel e1 NE e2) = isTrivially False (ERel e1 EQU e2)
isTrivially False (ERel e1 NE e2) = isTrivially True (ERel e1 EQU e2)
isTrivially res (ERel e1 op e2) =
  let val1 = evalConstExpr e1 in
    let val2 = evalConstExpr e2 in
      res == fromMaybe False (mapOp op <$> val1 <*> val2)
isTrivially _ _ = False

hasNoSideEffects :: Expr -> Bool
hasNoSideEffects (EApp _ _) = False
hasNoSideEffects (EMeth _ _ _) = False
hasNoSideEffects (Neg e) = hasNoSideEffects e
hasNoSideEffects (Not e) = hasNoSideEffects e
hasNoSideEffects (EMul e1 op e2) =
     hasNoSideEffects e1
  && hasNoSideEffects e2
  && (op == Times || evalConstExpr e2 /= Just 0)
hasNoSideEffects (EAdd e1 _ e2) = hasNoSideEffects e1 && hasNoSideEffects e2
hasNoSideEffects (ERel e1 _ e2) = hasNoSideEffects e1 && hasNoSideEffects e2
hasNoSideEffects (EAnd e1 e2) = hasNoSideEffects e1 && hasNoSideEffects e2
hasNoSideEffects (EOr e1 e2) = hasNoSideEffects e1 && hasNoSideEffects e2
hasNoSideEffects _ = True

evalConstExpr :: Expr -> Maybe Integer
evalConstExpr (ELitInt i) = Just i
evalConstExpr (EMul e1 Times e2) = (*) <$> evalConstExpr e1 <*> evalConstExpr e2
evalConstExpr (EMul e1 Div e2) =
  div <$> evalConstExpr e1 <*> checkNonZero (evalConstExpr e2)
evalConstExpr (EMul e1 Mod e2) =
  rem <$> evalConstExpr e1 <*> checkNonZero (evalConstExpr e2)
evalConstExpr (EAdd e1 Plus e2) = (+) <$> evalConstExpr e1 <*> evalConstExpr e2
evalConstExpr (EAdd e1 Minus e2) = (-) <$> evalConstExpr e1 <*> evalConstExpr e2
evalConstExpr _ = Nothing

checkNonZero :: (Num a, Ord a)  => Maybe a -> Maybe a
checkNonZero (Just 0) = Nothing
checkNonZero m = m

mapOp :: Ord a => RelOp -> a -> a -> Bool
mapOp LTH = (<)
mapOp LE = (<=)
mapOp GTH = (>)
mapOp GE = (>=)
mapOp _ = (\_ _ -> False)

itemName :: Item -> Ident
itemName (NoInit id) = id
itemName (Init id _) = id

mapExpr :: (Expr -> [a]) -> Expr -> [a]
mapExpr f = go
  where
    go e = f e ++
      case e of
        EApp _ args    -> concatMap go args
        EMeth _ _ args -> concatMap go args
        Neg expr       -> go expr
        Not expr       -> go expr
        EMul e1 _ e2   -> go e1 ++ go e2
        EAdd e1 _ e2   -> go e1 ++ go e2
        ERel e1 _ e2   -> go e1 ++ go e2
        EAnd  e1 e2    -> go e1 ++ go e2
        EOr  e1 e2     -> go e1 ++ go e2
        _              -> []
mapStmt :: (Stmt -> [a]) -> Stmt -> [a]
mapStmt f = go
  where
    go s = f s ++
      case s of
        Cond _ s'         -> go s'
        CondElse _ s1 s2  -> go s1 ++ go s2
        While _ s'        -> go s'
        BStmt (Block s's) -> concatMap go s's
        _                 -> []

getArgName :: Arg -> Ident
getArgName (Arg _ name) = name

mainID :: Ident
mainID = Ident "main"

askMap :: (Ord a, MonadReader r m) => a -> (r -> Map a b) -> m b
askMap x f = asks $ (! x) . f

getMap :: (Ord a, MonadState s m) => a -> (s -> Map a b) -> m b
getMap x f = gets $ (! x) . f


builtins :: [Ident]
builtins = map Ident [ "printInt"
                     , "printString"
                     , "error"
                     , "readInt"
                     , "readString"
                     ]

getBinOp :: Op -> Integer -> Integer -> Integer
getBinOp (OAdd Plus) = (+)
getBinOp (OAdd Minus) = (-)
getBinOp (OMul Times) = (*)
getBinOp (OMul Div) = div
getBinOp (OMul Mod) = rem
getBinOp OAnd = (.&.)
getBinOp OOr = (.|.)
getBinOp (ORel LTH) = fromBoolOp (<)
getBinOp (ORel LE) = fromBoolOp (<=)
getBinOp (ORel GTH) = fromBoolOp (>)
getBinOp (ORel GE) = fromBoolOp (>=)
getBinOp (ORel EQU) = fromBoolOp (==)
getBinOp (ORel NE) = fromBoolOp (/=)

fromBoolOp :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer
fromBoolOp op a b = fromBool $ op a b

mreplace :: (Ord a, MonadReader (Map a a) m) => a -> m a
mreplace v = asks $ Map.findWithDefault v v

localUnion :: (Ord k, MonadReader (Map k v) m) => Map k v -> m a -> m a
localUnion = local . Map.union

getClass :: ClassMappable m => Ident -> m Class
getClass id = (!id) <$> getClassMap

getAllFields :: ClassMappable m => Ident -> m [(Ident, Type)]
getAllFields name = if name == noBaseClass then return [] else do
  cls@(Class _ base mems) <- getClass name
  let fldNames = getFields cls
      flds = zip fldNames $ map (mems!) fldNames
  rest <- getAllFields base
  return $ rest ++ flds

getAllMethods :: ClassMappable m => Ident -> m [(Ident, Ident)] -- pair of mangled, unmangled name
getAllMethods name = if name == noBaseClass then return [] else do
  cls@(Class _ base _) <- getClass name
  let methNames = getMethods cls
      mangledNames = map (mangler name) methNames
  rest <- getAllMethods base
  let rest' = filter (not . (`elem` methNames)) $ map snd rest
      rest'' = zip (map (mangler name) rest') rest'
  return $ rest'' ++ zip mangledNames methNames

isAMethod :: ClassMappable m => Ident -> Ident -> m Bool
isAMethod methName clsName = (methName `elem`) . map fst <$> getAllMethods clsName

getMethods :: Class -> [Ident]
getMethods = map fst . filter (not . isField . snd) . Map.toList . classMembers

getFields :: Class -> [Ident]
getFields = map fst . filter (isField . snd) . Map.toList . classMembers

isField :: Type -> Bool
isField (Fun _ _) = False
isField _ = True


methodMarker :: String
methodMarker = "__method__ "

markMethod :: Ident -> Ident
markMethod = mapIdent (methodMarker ++)

mangler :: Ident -> Ident -> Ident
mangler id1 = mapIdent _mangler
  where _mangler id2 = unIdent id1 ++ '_' : id2

mapIdent :: (String -> String) -> Ident -> Ident
mapIdent f (Ident x) = Ident (f x)

