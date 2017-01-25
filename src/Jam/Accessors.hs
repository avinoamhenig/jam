module Jam.Accessors (
  ExpPath (..), endExpPath,
  ChildExpIndex (..),
  appendExpPath,
  parentExpPath,
  childExp,
  childExpSetter,
  expAtPath,
  constructors,
  findIdFromPath,
  findRootId,
  finalType,
  arity
) where

import Prelude hiding (lookup)
import Util.IndexedMap
import qualified Data.Map as Map
import Jam.Ast

constructors (TyDef _ _ _ cs) = cs
arity (TyDef _ _ a _) = a

data ExpPath = RootExpPath
             | RootBindingExpPath Id ExpPath
             | BindingExpPath Id ExpPath
             | ChildExpPath ChildExpIndex ExpPath
             deriving (Eq, Show)
endExpPath = RootExpPath

data ChildExpIndex = LambdaBodyIndex
                   | AppFuncIndex
                   | AppArgIndex
                   | IfCondIndex
                   | IfThenIndex
                   | IfElseIndex
                   deriving (Eq, Show)

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
  case lookup ident $ bindings e of
    Nothing -> Nothing
    (Just bv) -> case bv of
      (ExpVal sub) -> subExpAtPath sub path
      _ -> Nothing
subExpAtPath e (ChildExpPath index path) =
  case childExp index e of
    Nothing -> Nothing
    (Just sub) -> subExpAtPath sub path

getArgU :: Exp -> Maybe Unique
getArgU (LambdaExp { argId = (Id u _ _)}) = Just u
getArgU _ = Nothing

findIdFromPath :: Prog -> Unique -> ExpPath -> Maybe Id
findIdFromPath prog idU path =
  case expAtPath prog path of
    Nothing -> Nothing
    Just e ->
      if (getArgU e) == Just idU
        then Just $ argId e
        else case foldWithKey (_foldIdHelper idU) Nothing $ bindings e of
          Nothing -> case path of
            RootExpPath            -> findRootId prog idU
            RootBindingExpPath _ _ -> findRootId prog idU
            _ -> findIdFromPath prog idU $ parentExpPath path
          Just ident -> Just ident

_foldIdHelper _ _ _ (Just ident) = Just ident
_foldIdHelper idU ident@(Id u _ _) _ _
  | u == idU = Just ident
  | otherwise = Nothing

findRootId :: Prog -> Unique -> Maybe Id
findRootId prog idU =
  foldWithKey (_foldIdHelper idU) Nothing $ rootBindings prog

childExp LambdaBodyIndex e@(LambdaExp {}) = Just $ body e
childExp AppFuncIndex e@(AppExp {}) = Just $ func e
childExp AppArgIndex e@(AppExp {}) = Just $ argVal e
childExp IfCondIndex e@(IfExp {}) = Just $ condExp e
childExp IfThenIndex e@(IfExp {}) = Just $ thenExp e
childExp IfElseIndex e@(IfExp {}) = Just $ elseExp e
childExp _ _ = Nothing

childExpSetter LambdaBodyIndex e@(LambdaExp{}) =
  Just $ (\c -> e { body = c })
childExpSetter AppFuncIndex e@(AppExp{}) = Just $ (\c -> e { func = c })
childExpSetter AppArgIndex e@(AppExp{}) = Just $ (\c -> e { argVal = c })
childExpSetter IfCondIndex e@(IfExp{}) = Just $ (\c -> e { condExp = c })
childExpSetter IfThenIndex e@(IfExp{}) = Just $ (\c -> e { thenExp = c })
childExpSetter IfElseIndex e@(IfExp{}) = Just $ (\c -> e { elseExp = c })
childExpSetter _ _ = Nothing

finalType :: Prog -> Type -> Type
finalType p (TyVarType tv) = case Map.lookup tv (tyvarMap p) of
  Nothing -> TyVarType tv
  Just t -> finalType p t
finalType p (TyDefType td params) = TyDefType td $ (finalType p) <$> params
