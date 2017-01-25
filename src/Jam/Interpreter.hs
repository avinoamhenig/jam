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

interpret :: Prog -> ThrowsJamInterpreterError Prog
interpret = evalProg empty

type Env = Map Id BindingVal

extendEnv :: Env -> Id -> BindingVal -> Env
extendEnv env ident value = insert ident value env

mergeEnv :: Env -> Env -> Env
mergeEnv e1 e2 = unionWith (\_ b -> b) e1 e2

evalProg :: Env -> Prog -> ThrowsJamInterpreterError Prog
evalProg env prog = do newEnv <- newEnv
                       newRoot <- evalExp newEnv prog $ root prog
                       return $ prog { root = newRoot }
  where newEnv = evalBindings env prog $ rootBindings prog

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
          ExpVal e@(LambdaExp { capturedEnv = (Just ce) }) ->
            ExpVal $ e { capturedEnv = Just $ mergeEnv ce newEnv }
          e -> e

evalBindingVal :: Env -> Prog -> BindingVal
                    -> ThrowsJamInterpreterError BindingVal
evalBindingVal env prog (ExpVal e) = do e <- evalExp env prog e
                                        return $ ExpVal $ e
evalBindingVal _ _ bv = return bv

evalExp :: Env -> Prog -> Exp -> ThrowsJamInterpreterError Exp
evalExp environment prog e = case e of
  -- handle expressions without bindings
  BuiltInRef ref -> case lookup ref builtInFuncs of
                      Nothing -> error $ "Invalid built-in " ++ ref
                      Just e -> return e
  BuiltInExp _ -> return e

  -- handle expressions with bindings
  _ -> do
    env <- evalBindings environment prog $ bindings e
    case e of

      BottomExp {} -> throwError ReachedBottomExpression

      IdExp { ident = ident } ->
        case lookup ident env of
          Nothing -> error $ "Identifier does not exist: " ++ show ident
          Just bv -> case bv of
                      ExpVal val -> evalExp env prog val
                      TyDefDeconVal td -> return $
                          makeDeconFnExp env td (typeof e)
                      TyConVal _ -> return $ e { bindings = IM.empty }

      IfExp {} -> do cond <- evalExp env prog $ condExp e
                     if (ident cond) == (basisIds ! "True")
                       then evalExp env prog $ thenExp e
                       else evalExp env prog $ elseExp e

      LambdaExp {} -> case capturedEnv e of
                        Nothing -> return $ e { capturedEnv = Just env }
                        _ -> return $ e

      AppExp {} -> do
        f <- evalExp env prog $ func e
        x <- evalExp env prog $ argVal e
        case f of
          BuiltInExp bf -> evalExp env prog (bf x)
            -- eval reulst of  calling builting
            -- in case it is needed, like in
            -- case of a decon builtin
          LambdaExp {} ->
            case capturedEnv f of
              Nothing -> error "Evaluated lambda has no captured environment!"
              Just ce ->
                evalExp (extendEnv ce (argId f) $ ExpVal x)
                        prog
                        (body f)
          IdExp { ident = ident } ->
            case lookup ident env of
              Nothing -> error $ "Identifier does not exist: " ++ show ident
              Just bv ->
                case bv of
                  TyConVal _ -> return $ AppExp { func = f
                                                , argVal = x
                                                , typeof = typeof e
                                                , bindings = IM.empty
                                                }
                  _ -> error "Trying to apply non-applicable expression: " $
                        show f
          AppExp { typeof = t } -> return $ AppExp { func = f
                                                   , argVal = x
                                                   , typeof = t
                                                   , bindings = IM.empty
                                                   }
          _ -> error "Trying to apply non-applicable expression: " $ show f

      -- Atoms
      _ -> return $ e { bindings = IM.empty }

makeDeconFnExp :: Env -> TyDef -> Type -> Exp
makeDeconFnExp env (TyDef _ _ _ tyCons) (TyDefType _ [_, retT]) =
  _af2b ((length tyCons) + 1) [] $ \args -> case (args !! 0) of
    e@(AppExp {}) ->
      let f = getAppFn e
      in case f of
        IdExp { ident = ident } ->
          case lookup ident env of
            Nothing -> error $ "Identifier does not exist: " ++ show ident
            Just (TyConVal tyCon) ->
              (replaceAppFn e $ args !! ((_index tyCon tyCons) + 1)) {
                typeof = retT, bindings = IM.empty }
            _ -> error "Something is weird :/"
        _ -> error "Something went wrong"
    IdExp {ident = ident} ->
      case lookup ident env of
        Nothing -> error $ "Identifier does not exist: " ++ show ident
        Just (TyConVal tyCon) ->
          AppExp { func = args !! ((_index tyCon tyCons) + 1)
                 , argVal = createUnit'
                 , typeof = retT
                 , bindings = IM.empty
                 }
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
replaceAppFn e@(AppExp { func = f }) re = e { func = replaceAppFn f re }
replaceAppFn _ re = re

getAppFn :: Exp -> Exp
getAppFn (AppExp { func = f }) = getAppFn f
getAppFn f = f
