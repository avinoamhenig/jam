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

b2 f = ExpVal $ BuiltInExp (\x -> BuiltInExp $ f x)

basisIds = fromList [
    ( "False"      , Id "False" NoType      ),
    ( "True"       , Id "True"  NoType      ),
    ( "Bool.Decon" , Id "Bool.Decon" NoType ),
    ( "="          , Id "=" NoType          ),
    ( "+"          , Id "+" NoType          ),
    ( "-"          , Id "-" NoType          ),
    ( "*"          , Id "*" NoType          ),
    ( "/"          , Id "/" NoType          ),
    ( "%"          , Id "%" NoType          ),
    ( "<"          , Id "<" NoType          )
  ]

true = AppExp { func = createIdExp (basisIds ! "True")
                    , argVal = createUnit
                    , typeof = NoType
                    , bindings = empty
                    }
false = AppExp { func = createIdExp (basisIds ! "False")
                     , argVal = createUnit
                     , typeof = NoType
                     , bindings = empty
                     }

builtInFuncs = fromList [
  ("=", b2 (\x y -> if (value x) == (value y) then true else false )),
  ("+", b2 (\x y -> createNum $ (value x) + (value y) )),
  ("-", b2 (\x y -> createNum $ (value x) - (value y) )),
  ("*", b2 (\x y -> createNum $ (value x) * (value y) )),
  ("/", b2 (\x y -> createNum $ (value x) `quot` (value y) )),
  ("%", b2 (\x y -> createNum $ (value x) `mod` (value y) )),
  ("<", b2 (\x y -> if (value x) < (value y) then true else false ))
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
    ( basisIds ! "%", builtInFuncs ! "%" ),
    ( basisIds ! "<", builtInFuncs ! "<" )
  ]
