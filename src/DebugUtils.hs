
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
import BNFC.AbsLatte
import BNFC.ParLatte
import BNFC.ErrM

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

