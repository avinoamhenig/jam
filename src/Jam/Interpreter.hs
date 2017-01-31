module Jam.Interpreter (
  interpret
) where

import Prelude hiding (lookup)
import Jam.Ast
import Jam.Basis
import Jam.BuiltIns
import Jam.Creators
import qualified Util.IndexedMap as IM
import Data.Map
import Data.List (elemIndex)
import Jam.InterpreterError
import Control.Monad.Except

-- Interpret program.
interpret :: Prog -> ThrowsJamInterpreterError Prog
interpret = evalProg empty

-- Add a binding to an environment.
extendEnv :: Env -> Id -> BindingVal -> Env
extendEnv env ident value = insert ident value env

-- Merge two environments. The second env get precedent in case of conflicts.
mergeEnv :: Env -> Env -> Env
mergeEnv e1 e2 = unionWith (\_ b -> b) e1 e2

-- Evaluate a program given a starting environment.
evalProg :: Env -> Prog -> ThrowsJamInterpreterError Prog
evalProg env prog = do
  newEnv <- evalBindings env prog $ rootBindings prog
  newRoot <- evalExp newEnv prog $ root prog
  return $ prog { root = newRoot }

evalBindings :: Env -> Prog -> Bindings -> ThrowsJamInterpreterError Env
evalBindings env prog bindings = do
    foldedEnv <- IM.foldWithKey f (pure empty) bindings
    let newEnv = mergeEnv env $ Data.Map.map (updateCe newEnv) $ foldedEnv
    return newEnv
  where f :: Id -> BindingVal -> ThrowsJamInterpreterError Env
             -> ThrowsJamInterpreterError Env
        f ident bv newEnv = do
          newEnv <- newEnv
          newBv <- evalBindingVal (mergeEnv env newEnv) prog bv
          return $ extendEnv newEnv ident newBv
        updateCe newEnv bv = case bv of
          ExpVal (LambdaExp t b f x (Just ce)) ->
            ExpVal $ LambdaExp t b f x (Just $ mergeEnv ce newEnv)
          e -> e

evalBindingVal :: Env -> Prog -> BindingVal
                    -> ThrowsJamInterpreterError BindingVal
evalBindingVal env prog (ExpVal e) = do e <- evalExp env prog e
                                        return $ ExpVal $ e
evalBindingVal _ _ bv = return bv

evalExp :: Env -> Prog -> Exp -> ThrowsJamInterpreterError Exp
evalExp environment prog e = case e of
  BuiltInRef ref -> case lookup ref builtInFuncs of
                      Nothing -> error $ "Invalid built-in " ++ ref
                      Just e -> return e
  BuiltInExp _ -> return e
  UnitExp t _ -> return $ UnitExp t IM.empty
  NumExp t _ val -> return $ NumExp t IM.empty val
  BottomExp _ _ -> throwError ReachedBottomExpression

  IdExp expType bindings ident -> do
    env <- evalBindings environment prog bindings
    case lookup ident env of
      Nothing -> error $ "Identifier does not exist: " ++ show ident
      Just bv -> case bv of ExpVal val -> evalExp env prog val
                            TyDefDeconVal td -> return $
                              makeDeconFnExp env td expType
                            TyConVal _ -> return $ IdExp expType IM.empty ident

  IfExp _ bindings condExp thenExp elseExp -> do
    env <- evalBindings environment prog bindings
    cond <- evalExp env prog condExp
    if isTrue cond
      then evalExp env prog thenExp
      else evalExp env prog elseExp

  LambdaExp expType bindings argId body capturedEnv -> do
    env <- evalBindings environment prog bindings
    case capturedEnv of
      Nothing -> return $ LambdaExp expType IM.empty argId body (Just env)
      _       -> return e

  AppExp expType bindings func argVal -> do
    env <- evalBindings environment prog bindings
    f <- evalExp env prog func
    x <- evalExp env prog argVal
    case f of
      -- Eval result of calling built-in, in case it is needed (e.g. a decon
      -- builtin).
      BuiltInExp bf -> evalExp env prog (bf x)
      LambdaExp _ _ argId body capturedEnv ->
        case capturedEnv of
          Nothing -> error "Evaluated lambda has no captured environment!"
          Just ce -> evalExp (extendEnv ce argId $ ExpVal x) prog body
      IdExp _ _ ident ->
        case lookup ident env of
          Nothing -> error $ "Identifier does not exist: " ++ (show ident)
          Just bv ->
            case bv of
              TyConVal _ -> return $ AppExp expType IM.empty f x
              _ -> error $ "Trying to apply non-applicable expression: "
                           ++ (show f)
      AppExp t _ _ _ -> return $ AppExp t IM.empty f x
      _ -> error $ "Trying to apply non-applicable expression: " ++ (show f)

makeDeconFnExp :: Env -> TyDef -> Type -> Exp
makeDeconFnExp env (TyDef _ _ _ tyCons) (TyDefType _ [_, retT]) =
  _af2b ((length tyCons) + 1) [] $ \args -> case (args !! 0) of
    AppExp retT _ func arg ->
      let f = getAppFn func
      in case f of
        IdExp _ _ ident ->
          case lookup ident env of
            Nothing -> error $ "Identifier does not exist: " ++ show ident
            Just (TyConVal tyCon) -> replaceAppFn
                                      (AppExp retT IM.empty func arg)
                                      (args !! ((_index tyCon tyCons) + 1))
            _ -> error "Something is weird :/"
        _ -> error "Something went wrong"
    IdExp _ _ ident ->
      case lookup ident env of
        Nothing -> error $ "Identifier does not exist: " ++ show ident
        Just (TyConVal tyCon) ->
          AppExp retT IM.empty -- type, bindings
                 (args !! ((_index tyCon tyCons) + 1)) -- func
                 createUnit' -- argVal
        _ -> error "Something is weird :/"
    _ -> error "Something went wrong"
makeDeconFnExp _ _ _ = error "Something went wrong!"

_index a as = case elemIndex a as of
                Nothing -> error "Looking for something not in the list!"
                Just i -> i

_af2b :: Int -> [Exp] -> ([Exp] -> Exp) -> Exp
_af2b 1 args f = BuiltInExp (\x -> f (args ++ [x]))
_af2b n args f = BuiltInExp (\x -> _af2b (n-1) (args ++ [x]) f)

replaceAppFn :: Exp -> Exp -> Exp
replaceAppFn (AppExp ty binds f x) re = AppExp ty binds (replaceAppFn f re) x
replaceAppFn _ re = re

getAppFn :: Exp -> Exp
getAppFn (AppExp _ _ f _) = getAppFn f
getAppFn f = f
