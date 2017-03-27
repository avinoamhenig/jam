module Jam.Creators (
  createUnit, createUnit',
  createNum, createNum',
  createLambda,
  createIdExp,
  createApp,
  createIf
) where

import Util.IndexedMap
import Jam.Ast
import Jam.Accessors
import Jam.Basis
import Control.Monad.State
import qualified Data.Map as M

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
  u  <- getUnique
  u1 <- getUnique
  u2 <- getUnique
  let t1 = TyVarType $ TyVar u1 [ RootExpPath ]
      t2 = TyVarType $ TyVar u2 [ ChildExpPath LambdaBodyIndex RootExpPath ]
  return $ LambdaExp (TyDefType (functionType basis) [t1, t2]) -- type
                     empty                                     -- bindings
                     (Id u argName t1)                         -- argId
                     (_btm t2)                                 -- body
                     Nothing                                   -- capturedEnv

createIdExp :: Id -> State Prog Exp
createIdExp i@(Id _ _ t) = do
  newType <- instantiateTypeForId i t
  return $ IdExp newType empty i

createApp :: State Prog Exp
createApp = do
  u1 <- getUnique
  u2 <- getUnique
  let t1 = TyVarType $ TyVar u1 [ ChildExpPath AppArgIndex  RootExpPath
                                , ChildExpPath AppFuncIndex RootExpPath ]
      t2 = TyVarType $ TyVar u2 [ ChildExpPath AppFuncIndex RootExpPath ]
  return $ AppExp t2 empty -- type, bindings
                  (_btm $ TyDefType (functionType basis) [t1, t2]) -- func
                  (_btm t1) -- arg

createIf :: State Prog Exp
createIf = do
  u <- getUnique
  let t = TyVarType $ TyVar u [ ChildExpPath IfThenIndex RootExpPath
                              , ChildExpPath IfElseIndex RootExpPath ]
  return $ IfExp t empty -- type, bindings
                 (_btm $ TyDefType (boolType basis) []) -- condition
                 (_btm t) -- then
                 (_btm t) -- else

instantiateTypeForId :: Id -> Type -> State Prog Type
instantiateTypeForId ident (UniversalType t) =
  do p <- get
     (newT, _) <- _copyVars (finalType p t) M.empty
     return newT
  where _copyVars t@(TyVarType tv@(TyVar _ scopes)) tvMap =
          if any (\path -> not $ isInsideBinding path ident) scopes
            then return (t, tvMap)
            else case M.lookup tv tvMap of
                  Nothing -> do u <- getUnique
                                let newT = TyVarType (TyVar u [RootExpPath])
                                return $ (newT, M.insert tv newT tvMap)
                  Just newT -> return $ (newT, tvMap)
        _copyVars (TyDefType td paramTs) tvMap = do
          (newParamTs, newTvMap) <- copyParamTypes paramTs tvMap
          return (TyDefType td newParamTs, newTvMap)
        _copyVars t@(UniversalType _) tvMap = return (t, tvMap)
        copyParamTypes [] tvMap = return ([], tvMap)
        copyParamTypes (t:ts) tvMap = do
          (newT , newTvMap) <- _copyVars t tvMap
          (newTs, newTvMap) <- copyParamTypes ts newTvMap
          return (newT:newTs, newTvMap)
instantiateTypeForId _ t = return t
