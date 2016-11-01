module Lang.AST (
  Prog (..),
  BindingVal (..),
  Id (..),
  TyDef (..),
  TyCon (..),
  TyVar (..),
  Type (..),
  BuiltInReference,
  BuiltIn (..),
  Exp (..),
  Arity,
  Bindings,
) where

import Data.Map
import Data.Unique

type Bindings = Map Id BindingVal

data Prog = Prog { root :: Exp
                 , rootBindings :: Bindings
                 , tydefs :: [TyDef]
                 , tyvarMap :: Map TyVar Type
                 }

data BindingVal = ExpVal Exp
                | TyConVal TyCon
                | TyDefDeconVal TyDef
                | BuiltInVal BuiltIn

data Id = Id String Type
instance Show Id where show (Id name _) = name
instance Eq Id where (Id s1 _) == (Id s2 _) = s1 == s2
instance Ord Id where (Id s1 _) `compare` (Id s2 _) = s1 `compare` s2

data TyVar = TyVar Unique deriving (Ord, Eq)

type Arity = Integer
data TyDef = TyDef Arity [TyCon]

data TyCon = TyCon Type

data Type = TyDefType TyDef [Type]
          | TyVarType TyVar
          | NoType -- TODO: remove this

type BuiltInReference = String
data BuiltIn = BuiltIn BuiltInReference

data Exp = BottomExp { typeof :: Type
                     , bindings :: Bindings
                     }
         | UnitExp { typeof :: Type
                   , bindings :: Bindings
                   }
         | NumExp { value :: Integer
                  , typeof :: Type
                  , bindings :: Bindings
                  }
         | IdExp { ident :: Id
                 , bindings :: Bindings
                 , typeof :: Type
                 }
         | LambdaExp { argId :: Id
                     , body :: Exp
                     , bindings :: Bindings
                     , typeof :: Type
                     }
         | AppExp { func :: Exp
                  , argVal :: Exp
                  , typeof :: Type
                  , bindings :: Bindings
                  }
         | IfExp { condExp :: Exp
                 , thenExp :: Exp
                 , elseExp :: Exp
                 , typeof :: Type
                 , bindings :: Bindings
                 }

_deparen s
  | (s !! 0) == '(' = (take ((length s) - 2) . drop 1) s
  | otherwise = s
_wrapLet e s =
  let bindingStrs =
        foldWithKey
          (\(Id name _) val acc -> case val of
            ExpVal ev -> ("(" ++ name ++ " = "
                              ++ (_deparen $ show ev) ++ ")") : acc
            _ -> acc )
          [] $ bindings e
      letStr = unwords bindingStrs
  in if (length letStr) > 0
      then "(let " ++ letStr ++ " in " ++ (_deparen s) ++ ")"
      else s
instance Show Exp where -- TODO show bindings
  show e@(BottomExp {}) = _wrapLet e $ "_"
  show e@(UnitExp {}) = _wrapLet e $ "()"
  show e@(NumExp { value = val }) = _wrapLet e $ show val
  show e@(IdExp { ident = ident }) = _wrapLet e $ show ident
  show e@(LambdaExp {}) = _wrapLet e $  "(\\" ++ (show $ argId e)
                                              ++ " -> "
                                              ++ (_deparen . show $ body e)
                                              ++ ")"
  show e@(AppExp {}) = _wrapLet e $ "(" ++ (show $ func e) ++ " "
                                        ++ (show $ argVal e) ++ ")"
  show e@(IfExp {}) = _wrapLet e $ "(if " ++ (show $ condExp e) ++ " then "
                                          ++ (show $ thenExp e) ++ " else "
                                          ++ (show $ elseExp e) ++ ")"

instance Show Prog where show p = show $ root p
