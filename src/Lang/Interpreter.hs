module Lang.Interpreter (
  interpret
) where

import Prelude hiding (lookup)
import Lang.AST
import Lang.Basis
import Util.IndexedMap

interpret :: Prog -> Prog
interpret = evalProg empty

type Env = Bindings

extendEnv :: Env -> Id -> BindingVal -> Env
extendEnv env ident value = insert ident value env

evalProg :: Env -> Prog -> Prog
evalProg env prog = prog { root = evalExp newEnv prog $ root prog }
  where newEnv = evalBindings env prog $ rootBindings prog

evalBindings :: Env -> Prog -> Bindings -> Env
evalBindings env prog bindings = foldWithKey f env bindings
  where f ident bv newEnv = extendEnv newEnv ident $
                              evalBindingVal env prog bv

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
                    _ -> e { bindings = empty }

    IfExp {} -> let cond = evalExp env prog $ condExp e
                in if (ident $ func cond) == (basisIds ! "True")
                    then evalExp env prog $ thenExp e
                    else evalExp env prog $ elseExp e

    LambdaExp {} -> if (size $ capturedEnv e) == 0
                      then e { capturedEnv = env }
                      else e

    AppExp {} ->
      let f = evalExp env prog $ func e
          x = evalExp env prog $ argVal e
      in case f of
          BuiltInExp bf -> bf x
          LambdaExp {} ->
            evalExp (extendEnv (capturedEnv f) (argId f) $ ExpVal x)
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
                                       , bindings = empty
                                       }
                  -- TODO implement decon
                  TyDefDeconVal _ -> error "Decon not implemented yet"
                  _ -> error "Trying to apply non-applicable expression: " $
                        show f
          _ -> error "Trying to apply non-applicable expression: " $ show f

    -- Atoms
    BuiltInExp _ -> e
    _ -> e { bindings = empty }
