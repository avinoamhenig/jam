module Jts.Ast (
  IdName (..),
  ExpName (..),
  TyDefName (..),
  TyVarName,
  TyDefDesc (..),
  TypeDesc (..),
  ConsDef (..),
  ExpCreator (..),
  JtsCmd (..),
  JtsScript (..)
) where

import Data.List (intercalate)

newtype ExpName = ExpName String deriving (Eq, Ord)
newtype IdName = IdName String deriving (Eq, Ord)
newtype TyDefName = TyDefName String deriving (Eq, Ord)
type TyVarName = String

data ExpCreator = CrUnit
                | CrNum Int
                | CrLambda IdName ExpName
                | CrIdExp IdName
                | CrApp ExpName ExpName
                | CrIf ExpName ExpName ExpName

data JtsCmd = BndCmd ExpName IdName ExpName -- BndCmd scope idName valName
             | RplCmd ExpName ExpCreator
             | TypCmd [TyDefDesc]

data TyDefDesc = TyDefDesc TyDefName [TyVarName] [ConsDef]

data TypeDesc = TyVarTypeDesc TyVarName
              | TyDefTypeDesc TyDefName [TypeDesc]

data ConsDef = ConsDef IdName [TypeDesc]

newtype JtsScript = JtsScript [JtsCmd]

instance Show ExpName   where show (ExpName   s) = ":" ++ s
instance Show IdName    where show (IdName    s) = "$" ++ s
instance Show TyDefName where show (TyDefName s) = "#" ++ s
instance Show ExpCreator where
  show (CrUnit) = "Unit()"
  show (CrNum n) = "Num(" ++ (show n) ++ ")"
  show (CrLambda a b) = "Lambda(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (CrIdExp a) = "IdExp(" ++ (show a) ++ ")"
  show (CrApp a b) = "App(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (CrIf a b c) =
    "If(" ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
instance Show JtsCmd where
  show (BndCmd a b c) =
    "bnd " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c)
  show (RplCmd a b)   = "rpl " ++ (show a) ++ " " ++ (show b)
  show (TypCmd tds) = "typ " ++ (intercalate "  &  " $ map show tds)

instance Show TyDefDesc where
  show (TyDefDesc tdn tvns cds) =
    (show tdn) ++ " " ++ (concat $ map (++ " ") tvns) ++ "= "
               ++ (intercalate " | " $ map show cds)

instance Show ConsDef where
  show (ConsDef idn tds) = (show idn) ++ (concat $ map ((" " ++) . show) tds)
instance Show TypeDesc where
  show (TyVarTypeDesc tvn) = tvn
  show (TyDefTypeDesc tdn tds)
    | length tds == 0 = show tdn
    | otherwise = "(" ++ (show tdn) ++ " " ++ (unwords $ map show tds) ++ ")"
instance Show JtsScript where
  show (JtsScript cmds) = unlines $ map show cmds
