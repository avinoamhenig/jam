module Trns.AST (
  IdName (..),
  ExpName (..),
  ExpCreator (..),
  TrnsCmd (..),
  TrnsScript (..)
) where

newtype ExpName = ExpName String deriving (Eq, Ord)
newtype IdName = IdName String deriving (Eq, Ord)

data ExpCreator = CrUnit
                | CrNum Int
                | CrLambda IdName ExpName
                | CrIdExp IdName
                | CrApp ExpName ExpName
                | CrIf ExpName ExpName ExpName

data TrnsCmd = BndCmd ExpName IdName ExpName -- BndCmd scope idName valName
             | RplCmd ExpName ExpCreator

newtype TrnsScript = TrnsScript [TrnsCmd]

instance Show ExpName where show (ExpName s) = ":" ++ s
instance Show IdName  where show (IdName  s) = "$" ++ s
instance Show ExpCreator where
  show (CrUnit) = "Unit()"
  show (CrNum n) = "Num(" ++ (show n) ++ ")"
  show (CrLambda a b) = "Lambda(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (CrIdExp a) = "IdExp(" ++ (show a) ++ ")"
  show (CrApp a b) = "App(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (CrIf a b c) =
    "If(" ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ ")"
instance Show TrnsCmd where
  show (BndCmd a b c) =
    "bnd " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c)
  show (RplCmd a b)   = "rpl " ++ (show a) ++ " " ++ (show b)
instance Show TrnsScript where
  show (TrnsScript cmds) = unlines $ map show cmds
