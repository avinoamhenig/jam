module Jam.InterpreterError (
  JamInterpreterError (..),
  ThrowsJamInterpreterError,
) where

import Control.Monad.Except

data JamInterpreterError = ReachedBottomExpression
                           deriving (Show)

type ThrowsJamInterpreterError = Except JamInterpreterError
