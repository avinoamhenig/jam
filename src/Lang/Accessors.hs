module Lang.Accessors (
  ExpPath (..), endExpPath,
  ChildExpIndex (..),
  appendExpPath,
  parentExpPath,
  childExp,
  childExpSetter,
  expAtPath,
  constructors,
  findIdFromPath,
  findRootId
) where

import Prelude hiding (lookup)
import Util.IndexedMap
import Lang.AST

constructors (TyDef _ cs) = cs

data ExpPath = RootExpPath
             | RootBindingExpPath Id ExpPath
             | BindingExpPath Id ExpPath
             | ChildExpPath ChildExpIndex ExpPath
endExpPath = RootExpPath

data ChildExpIndex = LambdaBodyIndex
                   | AppFuncIndex
                   | AppArgIndex
                   | IfCondIndex
                   | IfThenIndex
                   | IfElseIndex

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

getArgName :: Exp -> Maybe String
getArgName (LambdaExp { argId = (Id name _)}) = Just name
getArgName _ = Nothing

findIdFromPath :: Prog -> String -> ExpPath -> Maybe Id
findIdFromPath prog idName path =
  case expAtPath prog path of
    Nothing -> Nothing
    Just e ->
      if (getArgName e) == Just idName
        then Just $ argId e
        else case foldWithKey (_foldIdHelper idName) Nothing $ bindings e of
          Nothing -> case path of
            RootExpPath            -> findRootId prog idName
            RootBindingExpPath _ _ -> findRootId prog idName
            _ -> findIdFromPath prog idName $ parentExpPath path
          Just ident -> Just ident

_foldIdHelper _ _ _ (Just ident) = Just ident
_foldIdHelper idName ident@(Id name _) _ _
  | name == idName = Just ident
  | otherwise = Nothing

findRootId :: Prog -> String -> Maybe Id
findRootId prog idName =
  foldWithKey (_foldIdHelper idName) Nothing $ rootBindings prog

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
