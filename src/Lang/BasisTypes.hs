module Lang.BasisTypes (
  functionType,
  unitType,
  numberType,
  boolType,
  basisTypes,
) where

import Lang.AST

functionType = TyDef "->"     2 []
unitType     = TyDef "Unit"   0 []
numberType   = TyDef "Number" 0 []
boolType     = TyDef "Bool"   0 [
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
