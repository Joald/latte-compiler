
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module DebugUtils where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.Char
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import BNFC.AbsLatte hiding (Int, Void, Bool)
import qualified BNFC.AbsLatte as Latte
import BNFC.ParLatte
import BNFC.ErrM
import CodeGen.CodeGen
import Types.Typechecker
import CodeGen.Abs

twice :: Monad m => m a -> m (a, a)
twice action = do
  v1 <- action
  v2 <- action
  return (v1, v2)


parse :: FilePath -> IO Program
parse s = do
  p <- readFile s
  let ts = myLexer p
  return ((\(Ok tr) -> tr) $ pProgram ts)

getMain :: Program -> [Stmt]
getMain (Program topDefs) = selectMain topDefs
selectMain :: [TopDef] -> [Stmt]
selectMain (TopFnDef (FnDef Latte.Int (Ident "main") [] (Block stmts)):_) = stmts
selectMain (_:tds) = selectMain tds

te :: IO ()
te = putStrLn "\n\n"

gen :: String -> IO ()
gen f = do
  p <- parse f
  let Right c = typeCheck p
      s = runCodeGen (compileProgram p) c
  putStrLn $ unlines $ map showQuad $ snd s
  te
  putStrLn $ unlines $ map show $ fst s
  te
  putStrLn $ codeGen c p 

