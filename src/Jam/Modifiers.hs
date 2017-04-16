module Jam.Modifiers (
  replace,
  bind,
  addTyDef,
  setType,
  prependScopes,
  unify
) where

import Prelude hiding (lookup)
import Jam.Ast
import Jam.Accessors
import Jam.Creators
import Util.IndexedMap
import qualified Data.Map as M
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except (throwError)
import Jam.Error

_bind :: Id -> BindingVal -> Exp -> Exp
_bind i v e = setBindings e $ insert i v $ getBindings e

bind :: String -> ExpPath -> StateThrowsJamError (Maybe Id)
bind idName scopePath = do
  prog <- get
  case expAtPath prog scopePath of
    Nothing -> return Nothing
    Just e -> do u1 <- liftState getUnique
                 u2 <- liftState getUnique
                 let t = TyVarType $ TyVar u1 [appendExpPath scopePath $
                                               BindingExpPath ident RootExpPath]
                     ident = Id u2 idName (UniversalType t ident)
                 prog <- get
                 prog <- lift . return $ _replace prog scopePath $
                                  _bind ident (ExpVal $ BottomExp t empty) e
                 put prog
                 return $ Just ident

addTyDef :: Prog -> TyDef -> Prog
addTyDef p td = p { tydefs = td:(tydefs p) }

_replaceSubExp :: Exp -> ExpPath -> Exp -> Exp
_replaceSubExp _ RootExpPath new = new
_replaceSubExp orig (RootBindingExpPath _ _) _ = orig
_replaceSubExp orig (BindingExpPath i path) new =
  case lookup i $ getBindings orig of
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

_replace :: Prog -> ExpPath -> Exp -> Prog
_replace p (RootBindingExpPath i path) new = p {
  rootBindings = case lookup i $ rootBindings p of
    Nothing -> rootBindings p
    Just bv -> case bv of
      ExpVal orig -> insert i (ExpVal $ _replaceSubExp orig path new) $
                      rootBindings p
      _ -> rootBindings p
  }
_replace p path new = p { root = _replaceSubExp (root p) path new }

replace :: Prog -> ExpPath -> Exp -> ThrowsJamError Prog
replace p path r =
  case expAtPath p path of
    Nothing -> throwError $ BadExpPath path
    Just e ->
      let newR = setBindings (prependPathToFrag path r)
                             (unionWith (\_ b -> b) (getBindings e)
                                                    (getBindings r))
          (unifies, newProg) = runState (unify (getType e) (getType newR)) p
      in if unifies
          then return $ _replace newProg path newR
          else throwError $ TypeMismatch (finalType p $ getType e)
                                         (finalType p $ getType r)

unify :: Type -> Type -> State Prog Bool
unify t1 t2 = do p <- get
                 unifies <- _unify (finalType p t1) (finalType p t2)
                 when (not unifies) $ put p
                 return unifies
  where
    _unify :: Type -> Type -> State Prog Bool
    _unify (TyVarType tv1@(TyVar _ s1)) (TyVarType tv2@(TyVar _ s2)) = do
      u <- getUnique
      let t3 = TyVarType $ TyVar u (s1 ++ s2)
      if tv1 == tv2 then return True
                    else do b1 <- _mapTv tv1 t3
                            b2 <- _mapTv tv2 t3
                            return (b1 && b2)
    _unify (TyVarType tv) t2 = do
      if typeContainsVar tv t2 -- occurs check (for cyclic types)
        then return False
        else do b <- _mapTv tv t2
                return b
    _unify t2 t1@(TyVarType _) = _unify t1 t2
    _unify (TyDefType td1 ps1) (TyDefType td2 ps2) =
      if td1 /= td2
      then return False
      else _unifyLists ps1 ps2
    _unify _ _ = error "Trying to unify general types."

    _mapTv :: TyVar -> Type -> State Prog Bool
    _mapTv tv t = do
      modify (\p -> p { tyVarMap = M.insert tv t (tyVarMap p) })
      p <- get
      bs <- mapM _unifyUnivConstraint
                 (Set.toList $ M.findWithDefault Set.empty tv (univTyVarMap p))
      return $ all id bs
    _unifyUnivConstraint :: UnivTyVarConstraint -> State Prog Bool
    _unifyUnivConstraint (UnivTyVarConstraint utv ntv ident) = do
      p <- get
      instT <- instantiateType (UniversalType (finalType p (TyVarType utv))
                                              ident)
      b <- _unify instT (finalType p (TyVarType ntv))
      return b

    _unifyLists :: [Type] -> [Type] -> State Prog Bool
    _unifyLists [] [] = return True
    _unifyLists (t1:ps1) (t2:ps2) = do
      p <- get
      unifies <- _unify (finalType p t1) (finalType p t2)
      if unifies then _unifyLists ps1 ps2
                 else return False
    _unifyLists _ _ = error "Types with same TyDef have different arity."

typeContainsVar :: TyVar -> Type -> Bool
typeContainsVar tv (TyVarType tv') = tv' == tv
typeContainsVar tv (TyDefType _ ts) = any (typeContainsVar tv) ts
typeContainsVar tv (UniversalType t _) = typeContainsVar tv t

setBindings :: Exp -> Bindings -> Exp
setBindings e binds = case e of
  BottomExp t _ -> BottomExp t binds
  UnitExp t _ -> UnitExp t binds
  NumExp t _ val -> NumExp t binds val
  IdExp t _ ident -> IdExp t binds ident
  LambdaExp t _ arg body cEnv -> LambdaExp t binds arg body cEnv
  AppExp t _ func arg -> AppExp t binds func arg
  IfExp t _ cExp tExp eExp -> IfExp t binds cExp tExp eExp
  BuiltInExp _ -> e
  BuiltInRef _ -> e

setType :: Exp -> Type -> Exp
setType e t = case e of
  BottomExp _ binds -> BottomExp t binds
  UnitExp _ binds -> UnitExp t binds
  NumExp _ binds val -> NumExp t binds val
  IdExp _ binds ident -> IdExp t binds ident
  LambdaExp _ binds arg body cEnv -> LambdaExp t binds arg body cEnv
  AppExp _ binds func arg -> AppExp t binds func arg
  IfExp _ binds cExp tExp eExp -> IfExp t binds cExp tExp eExp
  BuiltInExp _ -> e
  BuiltInRef _ -> e

-- Prepends the given path to the scopes of ALL tyvars in the given type.
prependScopes :: ExpPath -> Type -> Type
prependScopes path (TyVarType (TyVar u scopes)) =
  TyVarType $ TyVar u $ map (appendExpPath path) scopes
prependScopes path (TyDefType td paramTs) =
  TyDefType td $ map (prependScopes path) paramTs
prependScopes path (UniversalType t i) = UniversalType (prependScopes path t) i

prependPathToFrag :: ExpPath -> Exp -> Exp
prependPathToFrag path (BottomExp t bs) = BottomExp (prependScopes path t) bs
prependPathToFrag path (LambdaExp t bs (Id u s at) e ce) =
  LambdaExp (prependScopes path t) bs (Id u s (prependScopes path at))
            (prependPathToFrag path e) ce
prependPathToFrag path (AppExp t bs f a) =
  AppExp (prependScopes path t) bs (prependPathToFrag path f)
         (prependPathToFrag path a)
prependPathToFrag path (IfExp t bs ce te ee) =
  IfExp (prependScopes path t) bs (prependPathToFrag path ce)
        (prependPathToFrag path te) (prependPathToFrag path ee)
prependPathToFrag _ e = e
