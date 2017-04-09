module Jam.Ast (
  Prog (..),
  BindingVal (..),
  Id (..),
  TyDef (..),
  TyCon (..),
  TyVar (..),
  UnivTyVarConstraint (..),
  Type (..),
  Exp (..),
  Arity,
  Bindings,
  getUnique,
  getUniques,
  deparen,
  Unique,
  Env,
  ExpPath (..), endExpPath,
  ChildExpIndex (..),
  tyVars,
  isInsideBinding
) where

import Prelude hiding (lookup)
import qualified Util.IndexedMap
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Data.List (intercalate)

type Bindings = Util.IndexedMap.Map Id BindingVal
type Unique = Int

data Prog = Prog { root :: Exp
                 , rootBindings :: Bindings
                 , tydefs :: [TyDef]
                 , tyVarMap :: Map TyVar Type
                 , univTyVarMap :: Map TyVar (Set UnivTyVarConstraint)
                 , uniqC :: Unique
                 }

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

isInsideBinding :: ExpPath -> Id -> Bool
isInsideBinding RootExpPath _ = False
isInsideBinding (ChildExpPath _ childPath) i = isInsideBinding childPath i
isInsideBinding (RootBindingExpPath i' childPath) i
  | i == i' = True
  | otherwise = isInsideBinding childPath i
isInsideBinding (BindingExpPath i' childPath) i
  | i == i' = True
  | otherwise = isInsideBinding childPath i

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

data TyVar = TyVar Unique [ExpPath]
instance Show TyVar where show (TyVar u _) = "@" ++ (show u) -- ++ (show s)
instance Eq TyVar where (TyVar u1 _) == (TyVar u2 _) = u1 == u2
instance Ord TyVar where (TyVar u1 _) `compare` (TyVar u2 _) = u1 `compare` u2

data UnivTyVarConstraint = UnivTyVarConstraint TyVar TyVar Id
                           deriving (Eq, Ord)
instance Show UnivTyVarConstraint where
  show (UnivTyVarConstraint utv tv _) = (show utv) ++ " =>" ++ (show tv)

type Arity = Int
data TyDef = TyDef Unique String Arity [TyCon]
instance Show TyDef where show (TyDef _ name _ _) = name
instance Eq TyDef where (TyDef u1 _ _ _) == (TyDef u2 _ _ _) = u1 == u2

data TyCon = TyCon Unique String Type
instance Eq TyCon where (TyCon u1 _ _) == (TyCon u2 _ _) = u1 == u2

data Type = TyDefType TyDef [Type]
          | TyVarType TyVar
          | UniversalType Type Id

tyVars :: Type -> [TyVar]
tyVars (TyVarType tv) = [tv]
tyVars (TyDefType _ ts) = concat $ map tyVars ts
tyVars (UniversalType t _) = tyVars t

instance Show Type where
  show (TyVarType tv) = show tv
  show (TyDefType td params)
    | show td == "->" =
        let rt = deparen (show $ params !! 1)
            at = if isFnType (params !! 0)
                  then show (params !! 0)
                  else deparen $ show (params !! 0)
        in "(" ++ at ++ " -> " ++ rt ++ ")"
    | length params > 0 = "("
               ++ (show td) ++ " "
               ++ unwords (show <$> params)
               ++ ")"
    | otherwise = show td
  show (UniversalType t i) =
    let f = (\(TyVar _ scopes) -> all (\path -> isInsideBinding path i) scopes)
        uTvs = (Set.toList . Set.fromList) $ filter f (tyVars t)
    in "∀" ++ (intercalate "," (map show uTvs))
           ++ ".(" ++ (deparen $ show t) ++ ")"

isFnType :: Type -> Bool
isFnType (TyDefType td _) = (show td) == "->"
isFnType _ = False

-- Interpreter environment data structure. This stores a mapping
-- between identifiers and values.
type Env = Map Id BindingVal

data Exp = BuiltInExp (Exp -> Exp)
         | BuiltInRef String
         | BottomExp Type Bindings
         | UnitExp Type Bindings
         | NumExp Type Bindings Int
         | IdExp Type Bindings Id
         --                        arg  body  capturedEnv
         | LambdaExp Type Bindings Id   Exp   (Maybe Env)
         --                     func  arg
         | AppExp Type Bindings Exp   Exp
         --                    cond  then  else
         | IfExp Type Bindings Exp   Exp   Exp

deparen s
  | (s !! 0) == '(' && length s > 2 = (take ((length s) - 2) . drop 1) s
  | otherwise = s
_wrapLet bindings s =
  let bindingStrs =
        Util.IndexedMap.foldWithKey
          (\(Id _ name _) val acc -> case val of
            ExpVal ev -> ("(" ++ name ++ " = "
                              ++ (deparen $ show ev) ++ ")") : acc
            _ -> acc )
          [] $ bindings
      letStr = unwords bindingStrs
  in if (length letStr) > 0
      then "(let " ++ letStr ++ " in " ++ (deparen s) ++ ")"
      else s
instance Show Exp where
  show (BottomExp _ b) = _wrapLet b $ "_"
  show (UnitExp _ b) = _wrapLet b $ "()"
  show (NumExp _ b val) = _wrapLet b (show val)
  show (BuiltInRef n) = "<Built-in " ++ n ++ ">"
  show (BuiltInExp _) = "<Built-in>"
  show (IdExp _ b ident) = _wrapLet b $ show ident
  show (LambdaExp _ b arg body _) = _wrapLet b $  "(\\" ++ (show arg)
                                                 ++ " -> "
                                                 ++ (deparen $ show body)
                                                 ++ ")"
  show (AppExp _ b func arg) = _wrapLet b $ "(" ++ (show func) ++ " "
                                                ++ (show arg)  ++ ")"
  show (IfExp _ b c t e) = _wrapLet b $ "(if " ++ (show c) ++ " then "
                                               ++ (show t) ++ " else "
                                               ++ (show e) ++ ")"

instance Show Prog where show p = deparen $ show $ root p
