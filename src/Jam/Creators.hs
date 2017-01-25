module Jam.Creators (
  createUnit, createUnit',
  createNum, createNum',
  createLambda,
  createIdExp, createIdExp',
  createApp,
  createIf
) where

import Util.IndexedMap
import Jam.Ast
import Jam.Basis
import Control.Monad.State

_btm t = BottomExp { bindings = empty, typeof = t }

createUnit :: State Prog Exp
createUnit = return createUnit'

createUnit' :: Exp
createUnit' = UnitExp { bindings = empty
                      , typeof = TyDefType unitType []
                      }

createNum :: Int -> State Prog Exp
createNum x = return $ createNum' x

createNum' :: Int -> Exp
createNum' x = NumExp { value = x
                      , bindings = empty
                      , typeof = TyDefType numberType []
                      }

createLambda :: String -> State Prog Exp
createLambda argName = do
  u <- getUnique
  t1 <- (TyVarType . TyVar) <$> getUnique
  t2 <- (TyVarType . TyVar) <$> getUnique
  return $ LambdaExp { argId = Id u argName t1
                     , body  = _btm t2
                     , bindings = empty
                     , typeof = TyDefType functionType [t1, t2]
                     , capturedEnv = Nothing
                     }

createIdExp :: Id -> State Prog Exp
createIdExp i = return $ createIdExp' i

createIdExp' :: Id -> Exp
createIdExp' i@(Id _ _ t) = IdExp { ident = i
                                  , bindings = empty
                                  , typeof = t
                                  }

createApp :: State Prog Exp
createApp = do
  t1 <- (TyVarType . TyVar) <$> getUnique
  t2 <- (TyVarType . TyVar) <$> getUnique
  return $ AppExp { func = _btm $ TyDefType functionType [t1, t2]
                  , argVal = _btm t1
                  , bindings = empty
                  , typeof = t2
                  }

createIf :: State Prog Exp
createIf = do
  t <- (TyVarType . TyVar) <$> getUnique
  return $ IfExp { condExp = _btm $ TyDefType boolType []
                 , thenExp = _btm t
                 , elseExp = _btm t
                 , bindings = empty
                 , typeof = t
                 }
