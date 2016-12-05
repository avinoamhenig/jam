module Trns.Interpreter (
  Env (..),
  makeProgram,
  makeEnv,
  runCmd,
  runScript
) where

import Prelude hiding (lookup)
import Data.Map

import Lang.AST
import Trns.AST
import Lang.Creators
import Lang.Modifiers
import Lang.Accessors
import Lang.Basis
import Control.Monad.State

makeProgram = basisProg
makeEnv = Env { expNames = singleton (ExpName "root") RootExpPath
              , idNames = empty }

data Env = Env { expNames :: Map ExpName ExpPath
               , idNames  :: Map IdName  Id }

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
runCmd _ _ (TypCmd _ _ _) = error "Can't run typ command yet"

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
