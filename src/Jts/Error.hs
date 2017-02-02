module Jts.Error (
  JtsError (..),
  ThrowsJtsError,
  StateThrowsJtsError,
  liftState,
  fromJamE
) where

import Jts.Ast
import Jam.Ast
import Jam.Accessors (ExpPath)
import Jam.Error
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

data JtsError = BadExpLookup ExpName
              | BadIdLookup IdName
              | IdOutOfScope IdName ExpPath
              | BadTyVarLookup TyVarName
              | BadTyDefLookup TyDefName
              | EmptyTyDef TyDef
              | InvalidOperation JamError
              | UnknownError String
              deriving (Show)

type ThrowsJtsError = Except JtsError
type StateThrowsJtsError = StateT Prog ThrowsJtsError

fromJamE :: ThrowsJamError a -> ThrowsJtsError a
fromJamE je = case runExcept je of
  Left e -> ExceptT (Identity (Left $ InvalidOperation e))
  Right a -> ExceptT (Identity (Right a))
