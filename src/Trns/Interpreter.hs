module Trns.Interpreter (
  Env (..),
  makeProgram,
  makeEnv,
  runCmd,
  runScript
) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import qualified Util.IndexedMap as IM

import Lang.AST
import Trns.AST
import Lang.Creators
import Lang.Modifiers
import Lang.Accessors
import Lang.Basis
import Control.Monad.State

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

runScript :: Prog -> Env -> TrnsScript -> (Prog, Env)
runScript prog env (TrnsScript []) = (prog, env)
runScript prog env (TrnsScript (cmd:rest)) =
  let r = runCmd prog env cmd
  in runScript (fst r) (snd r) $ TrnsScript rest

runCmd :: Prog -> Env -> TrnsCmd -> (Prog, Env)
runCmd p env (BndCmd scopeName (IdName idName) valName) =
  case lookup scopeName $ expNames env of
    Nothing -> error $ "No Exp named " ++ (show scopeName)
    (Just path) ->
      let (mi, newProg) = runState (bind idName path) p
      in case mi of
        Nothing -> (newProg, env)
        Just ident -> ( newProg,
                        bindIdName (
                          bindExpName env valName $ appendExpPath path $
                            BindingExpPath ident RootExpPath)
                          (IdName idName) ident )
runCmd p env (RplCmd expName expCreator) =
  case lookup expName $ expNames env of
    Nothing -> error $ "No Exp named " ++ (show expName)
    (Just path) ->
      let ((newExp, newEnv), newProg) =
            runState (useCreator path env expCreator) p
      in (replace newProg path newExp, newEnv)
runCmd p env (TypCmd tdds) =
  let ((newEnv, _), newProg) = runState (typCmdToTyDefs env tdds) p
  in (newProg, newEnv)

useCreator :: ExpPath -> Env -> ExpCreator -> State Prog (Exp, Env)
useCreator _ env CrUnit = createUnit >>= \e -> return (e, env)
useCreator _ env (CrNum x) = createNum x >>= \e -> return (e, env)
useCreator path env (CrLambda (IdName idName) expName) =
  createLambda idName >>= \e -> return (
    e,
    bindIdName (bindExpName env expName $
                 appendExpPath path $ ChildExpPath LambdaBodyIndex endExpPath)
               (IdName idName) (argId e)
  )
useCreator path env (CrApp funcName argName) =
  createApp >>= \e -> return (
    e,
    let e1 = bindExpName env funcName $
              appendExpPath path $ ChildExpPath AppFuncIndex endExpPath
    in bindExpName e1 argName $
        appendExpPath path $ ChildExpPath AppArgIndex endExpPath
  )
useCreator path env (CrIf cName tName eName) =
  createIf >>= \e -> return (
    e,
    let e1 = bindExpName env cName $
              appendExpPath path $ ChildExpPath IfCondIndex endExpPath
        e2 = bindExpName e1 tName $
                  appendExpPath path $ ChildExpPath IfThenIndex endExpPath
    in bindExpName e2 eName $
        appendExpPath path $ ChildExpPath IfElseIndex endExpPath
  )
useCreator _ env (CrIdExp idName@(IdName name)) = do
  case lookup idName (idNames env) of
    Nothing -> case lookup name basisIds of
      Nothing -> error $ "No identifier named $" ++ name ++ " in scope"
      Just ident -> createIdExp ident >>= \e -> return (e, env)
    Just ident -> createIdExp ident >>= \e -> return (e, env)

typCmdToTyDefs :: Env -> [TyDefDesc] -> State Prog (Env, [TyDef])
typCmdToTyDefs env tdds = do
  tdUs <- getUniques (length tdds)
  let curTdMap = fromList $ map (\(u, (TyDefDesc tdn@(TyDefName n) _ _)) ->
                                        (tdn, TyDef u n 0 []))
                                (zip tdUs tdds)
  tyDefs <- mapM (tyDefDescToTyDef env curTdMap) (zip tdUs tdds)
  -- update tydefs
  -- let recursiveTyDefs = _updateRecTyDefs tdUs recursiveTyDefs tyDefs

  -- bind tycon identifiers
  newEnv <- bindTyConIds tyDefs env
  return (newEnv, tyDefs)

bindTyConIds :: [TyDef] -> Env -> State Prog Env
bindTyConIds [] env = return env
bindTyConIds (td@(TyDef _ name _ tcs):tds) env = do
  u1 <- getUnique
  u2 <- getUnique
  let deconT = deconType td (TyVar u1)
      deconName = name ++ ".Decon"
      deconId = Id u2 deconName deconT
  p <- get
  put $ p { rootBindings = IM.insert deconId (TyDefDeconVal td)
                                             (rootBindings p) }
  let env' = bindIdName env (IdName deconName) deconId
  newEnv <- bindTyConIds tds env'
  _bindTyconIds tcs newEnv
  where _bindTyconIds (tc@(TyCon _ name ty):tcs) env = do
          newEnv <- _bindTyconIds tcs env
          u <- getUnique
          p <- get
          let tcId = Id u name ty
          put $ p { rootBindings = IM.insert tcId (TyConVal tc)
                                             (rootBindings p) }
          return $ bindIdName newEnv (IdName name) tcId
        _bindTyconIds [] env = return env

deconType :: TyDef -> TyVar -> Type
deconType (TyDef _ _ _ tcs@((TyCon _ _ tcT):_)) tv =
  TyDefType functionType [finalReturnType tcT, _deconClauseTypes tcs tv]
  where _deconClauseTypes tcs tv =
          typesToFuncType (map (\(TyCon _ _ t) ->
                                  _wrapInFnTy $ conTyToDeconClauseType t tv)
                               tcs)
                          (TyVarType tv)
        _wrapInFnTy t@(TyDefType td _) | td == functionType = t
        _wrapInFnTy t = TyDefType functionType [(TyDefType unitType []), t]
deconType _ _ = error "Type definition has no constructors"

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
                        -> State Prog TyDef
tyDefDescToTyDef env curTdMap
                 (u, (TyDefDesc tdn@(TyDefName name) tvns cds)) =
  do let arity = length tvns
     tvUs <- getUniques arity
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
                      -> State Prog TyCon
consDefToTyCon env curTdMap tvMap td tvs (ConsDef (IdName idName) tDescs) = do
  tcU <- getUnique
  let types = map (tyDescToType env curTdMap tvMap) tDescs
      tyCon = TyCon tcU idName (typesToFuncType types $
                                 TyDefType td (map TyVarType tvs))
  return tyCon

tyDescToType :: Env -> Map TyDefName TyDef -> Map TyVarName TyVar -> TypeDesc
                    -> Type
tyDescToType _ _ tvMap (TyVarTypeDesc tvn) =
  case lookup tvn tvMap of
    Nothing -> error $ "No type variable " ++ tvn
    Just tv -> TyVarType tv
tyDescToType env curTdMap tvMap (TyDefTypeDesc tdn tds) =
  case lookup tdn (tyDefNames env) of
    Nothing -> case lookup tdn curTdMap of
      Nothing -> error $ "No such type " ++ (show tdn)
      Just td -> TyDefType td $ map (tyDescToType env curTdMap tvMap) tds
    Just td -> TyDefType td $ map (tyDescToType env curTdMap tvMap) tds

typesToFuncType :: [Type] -> Type -> Type
typesToFuncType [] rt = rt
typesToFuncType (t:ts) rt = TyDefType functionType [t, typesToFuncType ts rt]
