module Jam.BuiltIns (
  builtInFuncs
) where

import Data.Map
import Jam.Ast
import Jam.Creators
import Jam.Basis

b2 f = BuiltInExp (\x -> BuiltInExp $ f x)

true  = createIdExp' $ basisIds ! "True"
false = createIdExp' $ basisIds ! "False"

-- TODO: take a look at this. should builtins be able to throw errors?
_val (NumExp _ _ v) = v
_val e = error $ "Built-in expects a number but got: " ++ (show e)

builtInFuncs = fromList [
    ("=", b2 (\x y -> if (_val x) == (_val y) then true else false )),
    ("+", b2 (\x y -> createNum' $ (_val x) + (_val y) )),
    ("-", b2 (\x y -> createNum' $ (_val x) - (_val y) )),
    ("*", b2 (\x y -> createNum' $ (_val x) * (_val y) )),
    ("/", b2 (\x y -> createNum' $ (_val x) `quot` (_val y) )),
    ("%", b2 (\x y -> createNum' $ (_val x) `mod` (_val y) )),
    ("<", b2 (\x y -> if (_val x) < (_val y) then true else false ))
  ]
