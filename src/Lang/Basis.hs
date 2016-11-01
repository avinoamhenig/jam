module Lang.Basis (
  functionType,
  unitType,
  numberType,
  boolType,
  basisTypes,
  bindingNames,
  basisBindings
) where

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Lang.AST
import Lang.Accessors

functionType = TyDef 2 []
unitType     = TyDef 0 []
numberType   = TyDef 0 []
boolType     = TyDef 0 [
  TyCon (TyDefType functionType [ -- False
      TyDefType unitType [], TyDefType boolType []
    ]),
  TyCon (TyDefType functionType [ -- True
      TyDefType unitType [], TyDefType boolType []
    ])
  ]

basisTypes = [
    functionType,
    unitType,
    numberType,
    boolType
  ]

bindingNames = [
    "True", "False", "Bool.Decon",
    "=", "+", "-", "*", "/"
  ]

basisBindings = fromList [
    (Id "False" NoType, TyConVal ((constructors boolType) !! 0)),
    (Id "True"  NoType, TyConVal ((constructors boolType) !! 1)),
    (Id "Bool.Decon" NoType, TyDefDeconVal boolType),
    (Id "=" NoType, BuiltInVal $ BuiltIn "="),
    (Id "+" NoType, BuiltInVal $ BuiltIn "+"),
    (Id "-" NoType, BuiltInVal $ BuiltIn "-"),
    (Id "*" NoType, BuiltInVal $ BuiltIn "*"),
    (Id "/" NoType, BuiltInVal $ BuiltIn "/")
  ]
