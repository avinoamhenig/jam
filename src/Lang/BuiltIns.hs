module Lang.BuiltIns (
  builtInFuncs
) where

import Data.Map
import qualified Util.IndexedMap as IM
import Lang.AST
import Lang.Creators
import Lang.Basis

b2 f = BuiltInExp (\x -> BuiltInExp $ f x)

true = AppExp { func = createIdExp' $ basisIds ! "True"
              , argVal = createUnit'
              , typeof = TyDefType boolType []
              , bindings = IM.empty
              }
false = AppExp { func = createIdExp' $ basisIds ! "False"
               , argVal = createUnit'
               , typeof = TyDefType boolType []
               , bindings = IM.empty
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
