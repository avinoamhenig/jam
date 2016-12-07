module Lang.AST (
  Prog (..),
  BindingVal (..),
  Id (..),
  TyDef (..),
  TyCon (..),
  TyVar (..),
  Type (..),
  Exp (..),
  Arity,
  Bindings,
  getUnique,
  getUniques,
  deparen,
  Unique
) where

import qualified Util.IndexedMap
import Data.Map
import Control.Monad.State

type Bindings = Util.IndexedMap.Map Id BindingVal
type Unique = Int

data Prog = Prog { root :: Exp
                 , rootBindings :: Bindings
                 , tydefs :: [TyDef]
                 , tyvarMap :: Map TyVar Type
                 , uniqC :: Unique
                 }

getUnique :: State Prog Unique
getUnique = do p <- get
               let u = uniqC p
               put p { uniqC = u + 1 }
               return u

getUniques :: Int -> State Prog [Unique]
getUniques 0 = return []
getUniques n = do u <- getUnique
                  us <- getUniques (n-1)
                  return (u:us)

data BindingVal = ExpVal Exp
                | TyConVal TyCon
                | TyDefDeconVal TyDef

data Id = Id Unique String Type
instance Show Id where show (Id _ name _) = name
instance Eq Id where (Id u1 _ _) == (Id u2 _ _) = u1 == u2
instance Ord Id where (Id u1 _ _) `compare` (Id u2 _ _) = u1 `compare` u2

data TyVar = TyVar Unique deriving (Ord, Eq)
instance Show TyVar where show (TyVar u) = "@" ++ (show u)

type Arity = Int
data TyDef = TyDef Unique String Arity [TyCon]
instance Show TyDef where show (TyDef _ name _ _) = name
instance Eq TyDef where (TyDef u1 _ _ _) == (TyDef u2 _ _ _) = u1 == u2

data TyCon = TyCon Unique String Type

data Type = TyDefType TyDef [Type]
          | TyVarType TyVar
instance Show Type where
  show (TyVarType tv) = show tv
  show (TyDefType td params)
    | show td == "->" =
        let rt = if isFnType (params !! 1)
                 then deparen (show $ params !! 1)
                 else show $ params !! 1
        in "(" ++ (show $ params !! 0) ++ " -> " ++ rt ++ ")"
    | length params > 0 = "("
               ++ (show td) ++ " "
               ++ unwords (show <$> params)
               ++ ")"
    | otherwise = show td

isFnType :: Type -> Bool
isFnType (TyDefType td _) = (show td) == "->"
isFnType _ = False

data Exp = BuiltInExp (Exp -> Exp)
         | BuiltInRef String
         | BottomExp { typeof :: Type
                     , bindings :: Bindings
                     }
         | UnitExp   { typeof :: Type
                     , bindings :: Bindings
                     }
         | NumExp    { value :: Int
                     , typeof :: Type
                     , bindings :: Bindings
                     }
         | IdExp     { ident :: Id
                     , bindings :: Bindings
                     , typeof :: Type
                     }
         | LambdaExp { argId :: Id
                     , body :: Exp
                     , bindings :: Bindings
                     , typeof :: Type
                     , capturedEnv :: Maybe (Map Id BindingVal)
                     }
         | AppExp    { func :: Exp
                     , argVal :: Exp
                     , typeof :: Type
                     , bindings :: Bindings
                     }
         | IfExp     { condExp :: Exp
                     , thenExp :: Exp
                     , elseExp :: Exp
                     , typeof :: Type
                     , bindings :: Bindings
                     }

deparen s
  | (s !! 0) == '(' && length s > 2 = (take ((length s) - 2) . drop 1) s
  | otherwise = s
_wrapLet e s =
  let bindingStrs =
        Util.IndexedMap.foldWithKey
          (\(Id _ name _) val acc -> case val of
            ExpVal ev -> ("(" ++ name ++ " = "
                              ++ (deparen $ show ev) ++ ")") : acc
            _ -> acc )
          [] $ bindings e
      letStr = unwords bindingStrs
  in if (length letStr) > 0
      then "(let " ++ letStr ++ " in " ++ (deparen s) ++ ")"
      else s
instance Show Exp where
  show e@(BottomExp {}) = _wrapLet e $ "_"
  show e@(UnitExp {}) = _wrapLet e $ "()"
  show e@(NumExp { value = val }) = _wrapLet e (show val)
  show e@(BuiltInRef n) = _wrapLet e $ "<Built-in " ++ n ++ ">"
  show e@(BuiltInExp _) = _wrapLet e $ "<Built-in>"
  show e@(IdExp { ident = ident }) = _wrapLet e $ show ident
  show e@(LambdaExp {}) = _wrapLet e $  "(\\" ++ (show $ argId e)
                                              ++ " -> "
                                              ++ (deparen . show $ body e)
                                              ++ ")"
  show e@(AppExp {}) = _wrapLet e $ "(" ++ (show $ func e) ++ " "
                                        ++ (show $ argVal e) ++ ")"
  show e@(IfExp {}) = _wrapLet e $ "(if " ++ (show $ condExp e) ++ " then "
                                          ++ (show $ thenExp e) ++ " else "
                                          ++ (show $ elseExp e) ++ ")"

instance Show Prog where show p = deparen $ show $ root p
