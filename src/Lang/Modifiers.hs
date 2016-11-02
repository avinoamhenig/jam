module Lang.Modifiers (
  replace,
  bind
) where

import Prelude hiding (lookup)
import Lang.AST
import Lang.Accessors
import Util.IndexedMap

_bind :: Id -> BindingVal -> Exp -> Exp
_bind i v e = e { bindings = insert i v $ bindings e }

bind :: Prog -> String -> ExpPath -> (Prog, Maybe Id)
bind prog idName scopePath =
  case expAtPath prog scopePath of
    Nothing -> (prog, Nothing)
    Just e -> let ident = (Id idName NoType)
              in ( replace prog scopePath $
                    _bind ident (ExpVal $ BottomExp NoType empty) e
                 , Just ident
                 )

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
replace prog (RootBindingExpPath i path) new = prog {
  rootBindings = case lookup i $ rootBindings prog of
    Nothing -> rootBindings prog
    Just bv -> case bv of
      ExpVal orig -> insert i (ExpVal $ _replaceSubExp orig path new) $
                      rootBindings prog
      _ -> rootBindings prog
  }
replace prog path new = prog {
  root = _replaceSubExp (root prog) path new
  }
