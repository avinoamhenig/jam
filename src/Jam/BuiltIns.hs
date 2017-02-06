module Jam.BuiltIns (
  builtInFuncs
) where

import Data.Map
import Jam.Ast
import Jam.Creators
import Jam.Basis

builtInFuncs = fromList [
  ("=" , b2 (\x y -> if (numVal x) == (numVal y) then true else false )),
  ("!=", b2 (\x y -> if (numVal x) /= (numVal y) then true else false )),
  ("+" , b2 (\x y -> createNum' $ (numVal x) + (numVal y) )),
  ("-" , b2 (\x y -> createNum' $ (numVal x) - (numVal y) )),
  ("*" , b2 (\x y -> createNum' $ (numVal x) * (numVal y) )),
  ("/" , b2 (\x y -> createNum' $ (numVal x) `quot` (numVal y) )),
  ("%" , b2 (\x y -> createNum' $ (numVal x) `mod` (numVal y) )),
  ("<" , b2 (\x y -> if (numVal x) < (numVal y) then true else false )),
  (">" , b2 (\x y -> if (numVal x) > (numVal y) then true else false )),
  ("<=", b2 (\x y -> if (numVal x) <= (numVal y) then true else false )),
  (">=", b2 (\x y -> if (numVal x) >= (numVal y) then true else false ))
 ]

b2 f = BuiltInExp (\x -> BuiltInExp $ f x)

true  = createIdExp' $ (basisIds basis) ! "True"
false = createIdExp' $ (basisIds basis) ! "False"

-- TODO: take a look at this. should builtins be able to throw errors?
numVal (NumExp _ _ v) = v
numVal e = error $ "Built-in expects a number but got: " ++ (show e)
