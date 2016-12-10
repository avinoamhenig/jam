module Lang.BuiltIns (
  builtInFuncs
) where

import Data.Map
import Lang.AST
import Lang.Creators
import Lang.Basis

b2 f = BuiltInExp (\x -> BuiltInExp $ f x)

true  = createIdExp' $ basisIds ! "True"
false = createIdExp' $ basisIds ! "False"

builtInFuncs = fromList [
    ("=", b2 (\x y -> if (value x) == (value y) then true else false )),
    ("+", b2 (\x y -> createNum' $ (value x) + (value y) )),
    ("-", b2 (\x y -> createNum' $ (value x) - (value y) )),
    ("*", b2 (\x y -> createNum' $ (value x) * (value y) )),
    ("/", b2 (\x y -> createNum' $ (value x) `quot` (value y) )),
    ("%", b2 (\x y -> createNum' $ (value x) `mod` (value y) )),
    ("<", b2 (\x y -> if (value x) < (value y) then true else false ))
  ]
