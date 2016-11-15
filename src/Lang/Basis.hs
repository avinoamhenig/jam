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
import Lang.BasisTypes

b2 f = ExpVal $ BuiltInExp (\x -> BuiltInExp $ f x)

basisIds = fromList [
    ( "False"      , Id "False" tBool       ),
    ( "True"       , Id "True"  tBool       ),
    -- ( "Bool.Decon" , Id "Bool.Decon" NoType ),
    ( "="          , Id "=" tBinNum         ),
    ( "+"          , Id "+" tBinNum         ),
    ( "-"          , Id "-" tBinNum         ),
    ( "*"          , Id "*" tBinNum         ),
    ( "/"          , Id "/" tBinNum         ),
    ( "%"          , Id "%" tBinNum         ),
    ( "<"          , Id "<" tBinNum         )
  ]

tBool = TyDefType boolType []
tBinNum = TyDefType functionType [
  TyDefType numberType [],
  TyDefType functionType [ TyDefType numberType [],
                           TyDefType numberType [] ] ]

true = AppExp { func = createIdExp' (basisIds ! "True")
              , argVal = createUnit'
              , typeof = tBool
              , bindings = empty
              }
false = AppExp { func = createIdExp' (basisIds ! "False")
               , argVal = createUnit'
               , typeof = tBool
               , bindings = empty
               }

builtInFuncs = fromList [
  ("=", b2 (\x y -> if (value x) == (value y) then true else false )),
  ("+", b2 (\x y -> createNum' $ (value x) + (value y) )),
  ("-", b2 (\x y -> createNum' $ (value x) - (value y) )),
  ("*", b2 (\x y -> createNum' $ (value x) * (value y) )),
  ("/", b2 (\x y -> createNum' $ (value x) `quot` (value y) )),
  ("%", b2 (\x y -> createNum' $ (value x) `mod` (value y) )),
  ("<", b2 (\x y -> if (value x) < (value y) then true else false ))
  ]

basisBindings = fromList [
    ( basisIds ! "False", TyConVal ((constructors boolType) !! 0) ),
    ( basisIds ! "True", TyConVal ((constructors boolType) !! 1) ),
    -- ( basisIds ! "Bool.Decon", TyDefDeconVal boolType ),
    ( basisIds ! "=", builtInFuncs ! "=" ),
    ( basisIds ! "+", builtInFuncs ! "+" ),
    ( basisIds ! "-", builtInFuncs ! "-" ),
    ( basisIds ! "*", builtInFuncs ! "*" ),
    ( basisIds ! "/", builtInFuncs ! "/" ),
    ( basisIds ! "%", builtInFuncs ! "%" ),
    ( basisIds ! "<", builtInFuncs ! "<" )
  ]
