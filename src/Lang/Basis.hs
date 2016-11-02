module Lang.Basis (
  functionType,
  unitType,
  numberType,
  boolType,
  basisTypes,
  basisBindings,
  basisIds
) where

import Prelude hiding (lookup)
import Util.IndexedMap
import Lang.AST
import Lang.Accessors
import Lang.Creators

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

be f = ExpVal $ BuiltInExp f
b f = BuiltInExp f

basisIds = fromList [
    ( "False"      , Id "False" NoType      ),
    ( "True"       , Id "True"  NoType      ),
    ( "Bool.Decon" , Id "Bool.Decon" NoType ),
    ( "="          , Id "=" NoType          ),
    ( "+"          , Id "+" NoType          ),
    ( "-"          , Id "-" NoType          ),
    ( "*"          , Id "*" NoType          ),
    ( "/"          , Id "/" NoType          ),
    ( "%"          , Id "%" NoType          )
  ]

builtInFuncs = fromList [
  ("=", be (\x -> b (\y ->
          let c = if (value x) == (value y) then "True" else "False"
          in  AppExp { func = createIdExp (basisIds ! c)
                     , argVal = createUnit
                     , typeof = NoType
                     , bindings = empty
                     }
  ))),
  ("+", be (\x -> b (\y -> createNum $ (value x) + (value y) ))),
  ("-", be (\x -> b (\y -> createNum $ (value x) - (value y) ))),
  ("*", be (\x -> b (\y -> createNum $ (value x) * (value y) ))),
  ("/", be (\x -> b (\y -> createNum $ (value x) `quot` (value y) ))),
  ("%", be (\x -> b (\y -> createNum $ (value x) `mod` (value y) )))
  ]

basisBindings = fromList [
    ( basisIds ! "False", TyConVal ((constructors boolType) !! 0) ),
    ( basisIds ! "True", TyConVal ((constructors boolType) !! 1) ),
    ( basisIds ! "Bool.Decon", TyDefDeconVal boolType ),
    ( basisIds ! "=", builtInFuncs ! "=" ),
    ( basisIds ! "+", builtInFuncs ! "+" ),
    ( basisIds ! "-", builtInFuncs ! "-" ),
    ( basisIds ! "*", builtInFuncs ! "*" ),
    ( basisIds ! "/", builtInFuncs ! "/" ),
    ( basisIds ! "%", builtInFuncs ! "%" )
  ]
