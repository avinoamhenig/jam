module Jam.Modifiers (
  replace,
  bind,
  addTyDef,
  unify
) where

import Prelude hiding (lookup)
import Jam.Ast
import Jam.Accessors
import Util.IndexedMap
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except (throwError)
import Jam.Error

_bind :: Id -> BindingVal -> Exp -> Exp
_bind i v e = e { bindings = insert i v $ bindings e }

bind :: String -> ExpPath -> StateThrowsJamError (Maybe Id)
bind idName scopePath = do
  prog <- get
  case expAtPath prog scopePath of
    Nothing -> return Nothing
    Just e -> do t <- liftState $ (TyVarType . TyVar) <$> getUnique
                 u <- liftState $ getUnique
                 let ident = Id u idName t
                 prog <- get
                 prog <- lift $ replace prog scopePath $
                                  _bind ident (ExpVal $ BottomExp t empty) e
                 put prog
                 return $ Just ident

addTyDef :: Prog -> TyDef -> Prog
addTyDef p td = p { tydefs = td:(tydefs p) }

_replaceSubExp :: Exp -> ExpPath -> Exp -> Exp
_replaceSubExp _ RootExpPath new = new
_replaceSubExp orig (RootBindingExpPath _ _) _ = orig
_replaceSubExp orig (BindingExpPath i path) new =
  case lookup i $ bindings orig of
    Nothing -> orig
    Just bv -> case bv of
      ExpVal e -> _bind i (ExpVal $ _replaceSubExp e path new) orig
      _ -> orig
_replaceSubExp orig (ChildExpPath index path) new =
  case childExp index orig of
    Nothing -> orig
    Just child -> case childExpSetter index orig of
      Nothing -> orig
      Just setter -> setter $ _replaceSubExp child path new


copyType :: Type -> State Prog Type
copyType ty = do (newT, _) <- _copyType ty M.empty
                 return newT
  where _copyType (TyVarType tv) tvMap =
            case M.lookup tv tvMap of
              Nothing -> do u <- getUnique
                            let newT = TyVarType (TyVar u)
                            return $ (newT, M.insert tv newT tvMap)
              Just newT -> return $ (newT, tvMap)

        _copyType (TyDefType td paramTs) tvMap = do
            (newParamTs, newTvMap) <- copyParamTypes paramTs tvMap
            return (TyDefType td newParamTs, newTvMap)
        copyParamTypes [] tvMap = return ([], tvMap)
        copyParamTypes (t:ts) tvMap = do
          (newT , newTvMap) <- _copyType t tvMap
          (newTs, newTvMap) <- copyParamTypes ts newTvMap
          return (newT:newTs, newTvMap)

replace :: Prog -> ExpPath -> Exp -> ThrowsJamError Prog
replace p path r =
  case expAtPath p path of
    Nothing -> throwError $ BadExpPath path
    Just e ->
      let parentPath = (parentExpPath path)
          (newTy, newProg) =
            case expAtPath p parentPath of
              Just (AppExp {}) ->
                if (appendExpPath parentPath
                                  (ChildExpPath AppFuncIndex RootExpPath))
                      == path
                then case r of
                       IdExp { typeof = ty } -> runState (copyType ty) p
                       _ -> (typeof r, p)
                else (typeof r, p)
              _ -> (typeof r, p)
          _r = r { typeof = newTy }
          (unifies, newestProg) =
            runState (unify (typeof e) (typeof _r)) newProg
      in if unifies then return $ _replace newestProg path _r
                    else throwError $ TypeMismatch (typeof e) (typeof _r)
  where
    _replace p (RootBindingExpPath i path) new = p {
      rootBindings = case lookup i $ rootBindings p of
        Nothing -> rootBindings p
        Just bv -> case bv of
          ExpVal orig -> insert i (ExpVal $ _replaceSubExp orig path new) $
                          rootBindings p
          _ -> rootBindings p
      }
    _replace p path new = p { root = _replaceSubExp (root p) path new }

unify :: Type -> Type -> State Prog Bool
unify t1 t2 = do p <- get
                 unifies <- _unify (finalType p t1) (finalType p t2)
                 when (not unifies) $ put p
                 return unifies
  where
    _unify (TyVarType tv1) (TyVarType tv2) = do
      t3 <- (TyVarType . TyVar) <$> getUnique
      p <- get
      put $ p { tyvarMap = M.insert tv1 t3 $
                            M.insert tv2 t3 $ tyvarMap p }
      return True
    _unify (TyVarType tv) t2 = do
      p <- get
      put $ p { tyvarMap = M.insert tv t2 $ tyvarMap p }
      return True
    _unify t2 t1@(TyVarType _) = _unify t1 t2
    _unify (TyDefType td1 ps1) (TyDefType td2 ps2) =
      if td1 /= td2
      then return False
      else _unifyLists ps1 ps2

    _unifyLists [] [] = return True
    _unifyLists (t1:ps1) (t2:ps2) = do
      p <- get
      unifies <- _unify (finalType p t1) (finalType p t2)
      if unifies then _unifyLists ps1 ps2
                 else return False
    _unifyLists _ _ = error "Types with same TyDef have different arity."
