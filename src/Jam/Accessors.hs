module Jam.Accessors (
  appendExpPath,
  parentExpPath,
  childExp,
  childExpSetter,
  expAtPath,
  constructors,
  findIdFromPath,
  findRootId,
  finalType,
  arity,
  getType,
  getBindings,
  isInsideBinding,
  isTyCon
) where

import Prelude hiding (lookup)
import Util.IndexedMap
import qualified Data.Map as Map
import Jam.Ast

constructors (TyDef _ _ _ cs) = cs
arity (TyDef _ _ a _) = a

getType :: Exp -> Type
getType e = case e of
  BottomExp ty _ -> ty
  UnitExp ty _ -> ty
  NumExp ty _ _ -> ty
  IdExp ty _ _ -> ty
  LambdaExp ty _ _ _ _ -> ty
  AppExp ty _ _ _ -> ty
  IfExp ty _ _ _ _ -> ty
  BuiltInExp _ -> error "Attempted to get type of BuiltInExp"
  BuiltInRef _ -> error "Attempted to get type of BuiltInRef"

isTyCon :: Id -> Bool                   -- the idea here is that any ident
isTyCon (Id _ _ (TyDefType _ _)) = True -- would have a tyvar as the first type
isTyCon _ = False                       -- (with type constraints specializing
                                        -- the type), except for constructors,
                                        -- which would have the tydef as
                                        -- the first type.

getBindings :: Exp -> Bindings
getBindings e = case e of
  BottomExp _ binds -> binds
  UnitExp _ binds -> binds
  NumExp _ binds _ -> binds
  IdExp _ binds _ -> binds
  LambdaExp _ binds _ _ _ -> binds
  AppExp _ binds _ _ -> binds
  IfExp _ binds _ _ _ -> binds
  BuiltInExp _ -> empty
  BuiltInRef _ -> empty

isInsideBinding :: ExpPath -> Id -> Bool
isInsideBinding RootExpPath _ = False
isInsideBinding (ChildExpPath _ childPath) i = isInsideBinding childPath i
isInsideBinding (RootBindingExpPath i' childPath) i
  | i == i' = True
  | otherwise = isInsideBinding childPath i
isInsideBinding (BindingExpPath i' childPath) i
  | i == i' = True
  | otherwise = isInsideBinding childPath i

appendExpPath :: ExpPath -> ExpPath -> ExpPath
appendExpPath RootExpPath p2 = p2
appendExpPath (RootBindingExpPath ident p) p2 =
  RootBindingExpPath ident $ appendExpPath p p2
appendExpPath (BindingExpPath ident p) p2 =
  BindingExpPath ident $ appendExpPath p p2
appendExpPath (ChildExpPath index p) p2 = ChildExpPath index $ appendExpPath p p2

parentExpPath :: ExpPath -> ExpPath
parentExpPath RootExpPath = RootExpPath
parentExpPath (RootBindingExpPath _ RootExpPath) = RootExpPath
parentExpPath (RootBindingExpPath ident sub) =
  RootBindingExpPath ident $ parentExpPath sub
parentExpPath (BindingExpPath _ RootExpPath) = RootExpPath
parentExpPath (BindingExpPath ident sub) =
  BindingExpPath ident $ parentExpPath sub
parentExpPath (ChildExpPath _ RootExpPath) = RootExpPath
parentExpPath (ChildExpPath index sub) =
  ChildExpPath index $ parentExpPath sub

expAtPath :: Prog -> ExpPath -> Maybe Exp
expAtPath p RootExpPath = Just $ root p
expAtPath p (RootBindingExpPath ident path) =
  case lookup ident $ rootBindings p of
    Nothing -> Nothing
    (Just bv) -> case bv of
      (ExpVal e) -> subExpAtPath e path
      _ -> Nothing
expAtPath prog path = subExpAtPath (root prog) path

subExpAtPath :: Exp -> ExpPath -> Maybe Exp
subExpAtPath e RootExpPath = Just e
subExpAtPath _ (RootBindingExpPath _ _) = Nothing
subExpAtPath e (BindingExpPath ident path) =
  case lookup ident $ getBindings e of
    Nothing -> Nothing
    (Just bv) -> case bv of
      (ExpVal sub) -> subExpAtPath sub path
      _ -> Nothing
subExpAtPath e (ChildExpPath index path) =
  case childExp index e of
    Nothing -> Nothing
    (Just sub) -> subExpAtPath sub path

argIdForUniq :: Exp -> Unique -> Maybe Id
argIdForUniq (LambdaExp _ _ ident@(Id u' _ _) _ _) u = if u == u'
                                                       then Just ident
                                                       else Nothing
argIdForUniq _ _ = Nothing

findIdFromPath :: Prog -> Unique -> ExpPath -> Maybe Id
findIdFromPath prog idU path =
  case expAtPath prog path of
    Nothing -> Nothing
    Just e ->
      case argIdForUniq e idU of
        Just ident -> Just ident
        Nothing ->
          case foldWithKey (_foldIdHelper idU) Nothing $ getBindings e of
            Just ident -> Just ident
            Nothing -> case path of
              RootExpPath            -> findRootId prog idU
              RootBindingExpPath _ _ -> findRootId prog idU
              _ -> findIdFromPath prog idU $ parentExpPath path

_foldIdHelper _ _ _ (Just ident) = Just ident
_foldIdHelper idU ident@(Id u _ _) _ _
  | u == idU = Just ident
  | otherwise = Nothing

findRootId :: Prog -> Unique -> Maybe Id
findRootId prog idU =
  foldWithKey (_foldIdHelper idU) Nothing $ rootBindings prog

childExp :: ChildExpIndex -> Exp -> Maybe Exp
childExp LambdaBodyIndex (LambdaExp _ _ _ body _) = Just body
childExp AppFuncIndex (AppExp _ _ func _) = Just func
childExp AppArgIndex (AppExp _ _ _ arg) = Just arg
childExp IfCondIndex (IfExp _ _ condExp _ _) = Just condExp
childExp IfThenIndex (IfExp _ _ _ thenExp _) = Just thenExp
childExp IfElseIndex (IfExp _ _ _ _ elseExp) = Just elseExp
childExp _ _ = Nothing

childExpSetter :: ChildExpIndex -> Exp -> Maybe (Exp -> Exp)
childExpSetter LambdaBodyIndex (LambdaExp ty binds arg _ capturedEnv) =
  Just $ (\c -> LambdaExp ty binds arg c capturedEnv)
childExpSetter AppFuncIndex (AppExp ty binds _ arg) =
  Just $ (\c -> AppExp ty binds c arg)
childExpSetter AppArgIndex (AppExp ty binds func _) =
  Just $ (\c -> AppExp ty binds func c)
childExpSetter IfCondIndex (IfExp ty binds _ thenExp elseExp) =
  Just $ (\c -> IfExp ty binds c thenExp elseExp)
childExpSetter IfThenIndex (IfExp ty binds condExp _ elseExp) =
  Just $ (\c -> IfExp ty binds condExp c elseExp)
childExpSetter IfElseIndex (IfExp ty binds condExp thenExp _) =
  Just $ (\c -> IfExp ty binds condExp thenExp c)
childExpSetter _ _ = Nothing

finalType :: Prog -> Type -> Type
finalType p (TyVarType tv) = case Map.lookup tv (tyvarMap p) of
  Nothing -> TyVarType tv
  Just t -> finalType p t
finalType p (TyDefType td params) = TyDefType td $ (finalType p) <$> params
finalType p (UniversalType t) = UniversalType $ finalType p t
