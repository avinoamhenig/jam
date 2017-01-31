module Jts.Interpreter (
  Env (..),
  makeProgram,
  makeEnv,
  runCmd,
  runScript
) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import qualified Util.IndexedMap as IM
import Jam.Ast hiding (Env)
import Jts.Ast
import Jam.Creators
import Jam.Modifiers
import Jam.Accessors
import Jam.Basis
import Jts.Error
import Control.Monad.Except (throwError)
import Control.Monad.State (runStateT, get, put)
import Control.Monad.Trans.Class (lift)

makeProgram = basisProg
makeEnv = Env { expNames = singleton (ExpName "root") RootExpPath
              , idNames = empty
              , tyDefNames = empty }

data Env = Env { expNames   :: Map ExpName   ExpPath
               , idNames    :: Map IdName    Id
               , tyDefNames :: Map TyDefName TyDef}

bindExpName :: Env -> ExpName -> ExpPath -> Env
bindExpName e name path = e { expNames = insert name path $ expNames e }

bindIdName :: Env -> IdName -> Id -> Env
bindIdName e name ident = e { idNames = insert name ident $ idNames e }

runScript :: Prog -> Env -> JtsScript -> ThrowsJtsError (Prog, Env)
runScript prog env (JtsScript []) = return (prog, env)
runScript prog env (JtsScript (cmd:rest)) = do
  r <- runCmd prog env cmd
  runScript (fst r) (snd r) $ JtsScript rest

runCmd :: Prog -> Env -> JtsCmd -> ThrowsJtsError (Prog, Env)
runCmd p env (BndCmd scopeName (IdName idName) valName) =
  case lookup scopeName $ expNames env of
    Nothing -> throwError $ BadExpLookup scopeName
    (Just path) -> do
      (mi, newProg) <- fromJamE $ runStateT (bind idName path) p
      case mi of
        Nothing -> return (newProg, env)
        Just ident -> return (
                        newProg,
                        bindIdName
                          (bindExpName env valName $ appendExpPath path $
                            BindingExpPath ident RootExpPath)
                          (IdName idName)
                          ident
                      )
runCmd p env (RplCmd expName expCreator) =
  case lookup expName $ expNames env of
    Nothing -> throwError $ BadExpLookup expName
    (Just path) -> do
      ((newExp, newEnv), newProg) <-
        runStateT (useCreator path env expCreator) p
      finalProg <- fromJamE $ replace newProg path newExp
      return (finalProg, newEnv)
runCmd p env (TypCmd tdds) = do
  ((newEnv, _), newProg) <- runStateT (typCmdToTyDefs env tdds) p
  return (newProg, newEnv)

useCreator :: ExpPath -> Env -> ExpCreator -> StateThrowsJtsError (Exp, Env)
useCreator _ env CrUnit = do e <- liftState createUnit
                             return (e, env)
useCreator _ env (CrNum x) = do e <- liftState $ createNum x
                                return (e, env)
useCreator path env (CrLambda (IdName idName) expName) = do
  e@(LambdaExp _ _ argId _ _) <- liftState $ createLambda idName
  return (
    e,
    bindIdName (bindExpName env expName $
                  appendExpPath path $
                    ChildExpPath LambdaBodyIndex endExpPath)
               (IdName idName) argId )
useCreator path env (CrApp funcName argName) = do
  e <- liftState createApp
  return (
    e,
    let e1 = bindExpName env funcName $
               appendExpPath path $ ChildExpPath AppFuncIndex endExpPath
    in bindExpName e1 argName $
        appendExpPath path $ ChildExpPath AppArgIndex endExpPath )
useCreator path env (CrIf cName tName eName) = do
  e <- liftState createIf
  return (
    e,
    let e1 = bindExpName env cName $
              appendExpPath path $ ChildExpPath IfCondIndex endExpPath
        e2 = bindExpName e1 tName $
                  appendExpPath path $ ChildExpPath IfThenIndex endExpPath
    in bindExpName e2 eName $
        appendExpPath path $ ChildExpPath IfElseIndex endExpPath )
useCreator _ env (CrIdExp idName@(IdName name)) = do
  case lookup idName (idNames env) of
    Nothing -> case lookup name basisIds of
      Nothing -> lift $ throwError $ BadIdLookup idName
      Just ident -> (liftState $ createIdExp ident) >>= \e -> return (e, env)
    Just ident -> (liftState $ createIdExp ident) >>= \e -> return (e, env)

typCmdToTyDefs :: Env -> [TyDefDesc] -> StateThrowsJtsError (Env, [TyDef])
typCmdToTyDefs env tdds = do
  tdUs <- liftState $ getUniques (length tdds)
  let curTdMap = fromList $ map (\(u, (TyDefDesc tdn@(TyDefName n) _ _)) ->
                                        (tdn, TyDef u n 0 []))
                                (zip tdUs tdds)
  tyDefs <- mapM (tyDefDescToTyDef env curTdMap) (zip tdUs tdds)
  -- update tydefs
  -- let recursiveTyDefs = _updateRecTyDefs tdUs recursiveTyDefs tyDefs

  -- bind tycon identifiers
  newEnv <- bindTyConIds tyDefs env
  return (newEnv, tyDefs)

bindTyConIds :: [TyDef] -> Env -> StateThrowsJtsError Env
bindTyConIds [] env = return env
bindTyConIds (td@(TyDef _ name _ tcs):tds) env = do
  u1 <- liftState getUnique
  u2 <- liftState getUnique
  deconT <- lift $ deconType td (TyVar u1)
  let deconName = name ++ ".Decon"
      deconId = Id u2 deconName deconT
  p <- get
  put $ p { rootBindings = IM.insert deconId (TyDefDeconVal td)
                                             (rootBindings p) }
  let env' = bindIdName env (IdName deconName) deconId
  newEnv <- bindTyConIds tds env'
  _bindTyconIds tcs newEnv
  where _bindTyconIds (tc@(TyCon _ name ty):tcs) env = do
          newEnv <- _bindTyconIds tcs env
          u <- liftState getUnique
          p <- get
          let tcId = Id u name ty
          put $ p { rootBindings = IM.insert tcId (TyConVal tc)
                                             (rootBindings p) }
          return $ bindIdName newEnv (IdName name) tcId
        _bindTyconIds [] env = return env

deconType :: TyDef -> TyVar -> ThrowsJtsError Type
deconType (TyDef _ _ _ tcs@((TyCon _ _ tcT):_)) tv = return $
  TyDefType functionType [finalReturnType tcT, _deconClauseTypes tcs tv]
  where _deconClauseTypes tcs tv =
          typesToFuncType (map (\(TyCon _ _ t) ->
                                  _wrapInFnTy $ conTyToDeconClauseType t tv)
                               tcs)
                          (TyVarType tv)
        _wrapInFnTy t@(TyDefType td _) | td == functionType = t
        _wrapInFnTy t = TyDefType functionType [(TyDefType unitType []), t]
deconType td _ = throwError $ EmptyTyDef td

finalReturnType :: Type -> Type
finalReturnType t@(TyDefType td paramTs)
  | td == functionType = finalReturnType (paramTs !! 1)
  | otherwise = t
finalReturnType t = t

conTyToDeconClauseType :: Type -> TyVar -> Type
conTyToDeconClauseType (TyVarType _) _ = error "Something is wrong"
conTyToDeconClauseType (TyDefType td paramTs) retTv
  | td == functionType = TyDefType functionType [
                            paramTs !! 0,
                            conTyToDeconClauseType (paramTs !! 1) retTv
                          ]
  | otherwise = TyVarType retTv

tyDefDescToTyDef :: Env -> Map TyDefName TyDef -> (Unique, TyDefDesc)
                        -> StateThrowsJtsError TyDef
tyDefDescToTyDef env curTdMap
                 (u, (TyDefDesc tdn@(TyDefName name) tvns cds)) =
  do let arity = length tvns
     tvUs <- liftState $ getUniques arity
     let tvs = map TyVar tvUs
         tvMap = fromList $ zip tvns tvs
         td = curTdMap ! tdn
     tyCons <- mapM (consDefToTyCon env curTdMap tvMap td tvs) cds
     return $ TyDef u name arity tyCons

consDefToTyCon :: Env -> Map TyDefName TyDef
                      -> Map TyVarName TyVar
                      -> TyDef
                      -> [TyVar]
                      -> ConsDef
                      -> StateThrowsJtsError TyCon
consDefToTyCon env curTdMap tvMap td tvs (ConsDef (IdName idName) tDescs) = do
  tcU <- liftState getUnique
  types <- lift $ mapM (tyDescToType env curTdMap tvMap) tDescs
  return $ TyCon tcU idName (typesToFuncType types $
                                 TyDefType td (map TyVarType tvs))

tyDescToType :: Env -> Map TyDefName TyDef -> Map TyVarName TyVar -> TypeDesc
                    -> ThrowsJtsError Type
tyDescToType _ _ tvMap (TyVarTypeDesc tvn) =
  case lookup tvn tvMap of
    Nothing -> throwError $ BadTyVarLookup tvn
    Just tv -> return $ TyVarType tv
tyDescToType env curTdMap tvMap (TyDefTypeDesc tdn tds) =
  case lookup tdn (tyDefNames env) of
    Nothing -> case lookup tdn curTdMap of
      Nothing -> throwError $ BadTyDefLookup tdn
      Just td -> do rec <- mapM (tyDescToType env curTdMap tvMap) tds
                    return $ TyDefType td rec
    Just td -> do rec <- mapM (tyDescToType env curTdMap tvMap) tds
                  return $ TyDefType td rec

typesToFuncType :: [Type] -> Type -> Type
typesToFuncType [] rt = rt
typesToFuncType (t:ts) rt = TyDefType functionType [t, typesToFuncType ts rt]
