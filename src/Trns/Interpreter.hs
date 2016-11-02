module Trns.Interpreter (
  Env,
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
import Lang.CreateProg
import Lang.Modifiers
import Lang.Accessors

makeProgram = createProg
makeEnv = Env { expNames = singleton (ExpName "root") RootExpPath }

newtype Env = Env { expNames :: Map ExpName ExpPath }

bindExpName :: Env -> ExpName -> ExpPath -> Env
bindExpName e name path = e { expNames = insert name path $ expNames e }

runScript :: Prog -> Env -> TrnsScript -> (Prog, Env)
runScript prog env (TrnsScript []) = (prog, env)
runScript prog env (TrnsScript (cmd:rest)) =
  let r = runCmd prog env cmd
  in runScript (fst r) (snd r) $ TrnsScript rest

runCmd :: Prog -> Env -> TrnsCmd -> (Prog, Env)
runCmd p env (BndCmd scopeName (IdName idName) valName) =
  case lookup scopeName $ expNames env of
    Nothing -> error $ "No Exp named " ++ (show scopeName)
    (Just path) -> case bind p idName path of
      (newProg, Nothing) -> (newProg, env)
      (newProg, (Just ident)) -> (
        newProg,
        bindExpName env valName $ appendExpPath path $
                                    BindingExpPath ident RootExpPath )
runCmd p env (RplCmd expName expCreator) =
  case lookup expName $ expNames env of
    Nothing -> error $ "No Exp named " ++ (show expName)
    (Just path) ->
      let expAndEnv = useCreator p path env expCreator
          newExp = fst expAndEnv
          newEnv = snd expAndEnv
      in (replace p path newExp, newEnv)

useCreator :: Prog -> ExpPath -> Env -> ExpCreator -> (Exp, Env)
useCreator _ _ env CrUnit = (createUnit, env)
useCreator _ _ env (CrNum x) = (createNum x, env)
useCreator _ path env (CrLambda (IdName idName) expName) = (
    createLambda idName,
    bindExpName env expName $
      appendExpPath path $ ChildExpPath LambdaBodyIndex endExpPath
  )
useCreator _ path env (CrApp funcName argName) = (
    createApp,
    let e1 = bindExpName env funcName $
              appendExpPath path $ ChildExpPath AppFuncIndex endExpPath
    in bindExpName e1 argName $
        appendExpPath path $ ChildExpPath AppArgIndex endExpPath
  )
useCreator _ path env (CrIf cName tName eName) = (
    createIf,
    let e1 = bindExpName env cName $
              appendExpPath path $ ChildExpPath IfCondIndex endExpPath
        e2 = bindExpName e1 tName $
                  appendExpPath path $ ChildExpPath IfThenIndex endExpPath
    in bindExpName e2 eName $
        appendExpPath path $ ChildExpPath IfElseIndex endExpPath
  )
useCreator prog path env (CrIdExp (IdName idName)) =
  case findIdFromPath prog idName path of
    Nothing -> error $ "No identifier named $" ++ idName ++ " in scope"
    Just ident -> (createIdExp ident, env)
