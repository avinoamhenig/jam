module Lang.Interpreter (
  interpret
) where

import Prelude hiding (lookup)
import Lang.AST
import Lang.Basis
import Lang.BuiltIns
import qualified Util.IndexedMap as IM
import Data.Map

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
                    _ -> e { bindings = IM.empty }

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
          BuiltInExp bf -> bf x
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
                  -- TODO implement decon
                  TyDefDeconVal _ -> error "Decon not implemented yet"
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
