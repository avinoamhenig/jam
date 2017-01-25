module Jam.Error (
  JamError (..),
  ThrowsJamError,
  StateThrowsJamError,
  liftState
) where

import Jam.Ast
import Jam.Accessors (ExpPath)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

data JamError = BadExpPath ExpPath
              | TypeMismatch Type Type
              | UnknownError String
              deriving (Show)

type ThrowsJamError = Except JamError
type StateThrowsJamError = StateT Prog ThrowsJamError

liftState :: State Prog a -> StateT Prog (Except e) a
liftState s = (StateT $ ExceptT . Identity . Right . (runState s))
