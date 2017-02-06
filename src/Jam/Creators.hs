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

_btm t = BottomExp t empty

createUnit :: State Prog Exp
createUnit = return createUnit'

createUnit' :: Exp
createUnit' = UnitExp (TyDefType (unitType basis) []) empty

createNum :: Int -> State Prog Exp
createNum x = return $ createNum' x

createNum' :: Int -> Exp
createNum' x = NumExp (TyDefType (numberType basis) []) empty x

createLambda :: String -> State Prog Exp
createLambda argName = do
  u <- getUnique
  t1 <- (TyVarType . TyVar) <$> getUnique
  t2 <- (TyVarType . TyVar) <$> getUnique
  return $ LambdaExp (TyDefType (functionType basis) [t1, t2]) -- type
                     empty                                     -- bindings
                     (Id u argName t1)                         -- argId
                     (_btm t2)                                 -- body
                     Nothing                                   -- capturedEnv

createIdExp :: Id -> State Prog Exp
createIdExp i = return $ createIdExp' i

createIdExp' :: Id -> Exp
createIdExp' i@(Id _ _ t) = IdExp t empty i

createApp :: State Prog Exp
createApp = do
  t1 <- (TyVarType . TyVar) <$> getUnique
  t2 <- (TyVarType . TyVar) <$> getUnique
  return $ AppExp t2 empty -- type, bindings
                  (_btm $ TyDefType (functionType basis) [t1, t2]) -- func
                  (_btm t1) -- arg

createIf :: State Prog Exp
createIf = do
  t <- (TyVarType . TyVar) <$> getUnique
  return $ IfExp t empty -- type, bindings
                 (_btm $ TyDefType (boolType basis) []) -- condition
                 (_btm t) -- then
                 (_btm t) -- else
