module Lang.Modifiers (
  replace,
  bind,
  unify
) where

import Prelude hiding (lookup)
import Lang.AST
import Lang.Accessors
import Util.IndexedMap
import qualified Data.Map as Map
import Control.Monad.State

_bind :: Id -> BindingVal -> Exp -> Exp
_bind i v e = e { bindings = insert i v $ bindings e }

bind :: String -> ExpPath -> State Prog (Maybe Id)
bind idName scopePath = do
  prog <- get
  case expAtPath prog scopePath of
    Nothing -> return Nothing
    Just e -> do t <- (TyVarType . TyVar) <$> getUnique
                 prog <- get
                 let ident = Id idName t
                 put $ replace prog scopePath $
                         _bind ident (ExpVal $ BottomExp t empty) e
                 return $ Just ident

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

replace :: Prog -> ExpPath -> Exp -> Prog
replace p path r =
  case expAtPath p path of
    Nothing -> error "Replacing non-existent path"
    Just e ->
      let (unifies, newProg) = runState (unify (typeof e) (typeof r)) p
      in if unifies then _replace newProg path r
                    else error "Types don't match"
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
      put $ p { tyvarMap = Map.insert tv1 t3 $
                            Map.insert tv2 t3 $ tyvarMap p }
      return True
    _unify (TyVarType tv) t2 = do
      p <- get
      put $ p { tyvarMap = Map.insert tv t2 $ tyvarMap p }
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
