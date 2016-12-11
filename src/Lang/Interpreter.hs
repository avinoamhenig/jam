module Lang.Interpreter (
  interpret
) where

import Prelude hiding (lookup)
import Lang.AST
import Lang.Basis
import Lang.BuiltIns
import Lang.Creators
import qualified Util.IndexedMap as IM
import Data.Map
import Data.List (elemIndex)

interpret :: Prog -> Prog
interpret = evalProg empty

type Env = Map Id BindingVal

extendEnv :: Env -> Id -> BindingVal -> Env
extendEnv env ident value = insert ident value env

mergeEnv :: Env -> Env -> Env
mergeEnv e1 e2 = unionWith (\_ b -> b) e1 e2

evalProg :: Env -> Prog -> Prog
evalProg env prog = prog { root = evalExp newEnv prog $ root prog }
  where newEnv = evalBindings env prog $ rootBindings prog

evalBindings :: Env -> Prog -> Bindings -> Env
evalBindings env prog bindings =
    let newEnv = mergeEnv env $
                  Data.Map.map (updateCe newEnv) $
                    IM.foldWithKey f empty bindings
    in newEnv
  where f ident bv newEnv = extendEnv newEnv ident $
                              evalBindingVal (mergeEnv env newEnv) prog bv
        updateCe newEnv bv = case bv of
          ExpVal e@(LambdaExp { capturedEnv = (Just ce) }) ->
            ExpVal $ e { capturedEnv = Just $ mergeEnv ce newEnv }
          e -> e

evalBindingVal :: Env -> Prog -> BindingVal -> BindingVal
evalBindingVal env prog (ExpVal e) = ExpVal $ evalExp env prog e
evalBindingVal _ _ bv = bv

evalExp :: Env -> Prog -> Exp -> Exp
evalExp environment prog e =
  let env = evalBindings environment prog $ bindings e
  in case e of

    BottomExp {} -> error "Reached bottom expression"

    IdExp { ident = ident } ->
      case lookup ident env of
        Nothing -> error $ "Identifier does not exist: " ++ show ident
        Just bv -> case bv of
                    ExpVal val -> evalExp env prog val
                    TyDefDeconVal td -> makeDeconFnExp env td (typeof e)
                    TyConVal _ -> e { bindings = IM.empty }

    IfExp {} -> let cond = evalExp env prog $ condExp e
                in if (ident cond) == (basisIds ! "True")
                   then evalExp env prog $ thenExp e
                   else evalExp env prog $ elseExp e

    LambdaExp {} -> case capturedEnv e of
                      Nothing -> e { capturedEnv = Just env }
                      _ -> e

    AppExp {} ->
      let f = evalExp env prog $ func e
          x = evalExp env prog $ argVal e
      in case f of
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
                  TyConVal _ -> AppExp { func = f
                                       , argVal = x
                                       , typeof = typeof e
                                       , bindings = IM.empty
                                       }
                  _ -> error "Trying to apply non-applicable expression: " $
                        show f
          AppExp { typeof = t } -> AppExp { func = f
                                          , argVal = x
                                          , typeof = t
                                          , bindings = IM.empty
                                          }
          _ -> error "Trying to apply non-applicable expression: " $ show f

    BuiltInRef ref -> case lookup ref builtInFuncs of
                        Nothing -> error $ "Invalid built-in " ++ ref
                        Just e -> e

    -- Atoms
    BuiltInExp _ -> e
    _ -> e { bindings = IM.empty }

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
