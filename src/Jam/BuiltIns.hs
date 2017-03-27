module Jam.BuiltIns (
  builtInFuncs
) where

import Data.Map
import qualified Util.IndexedMap as IM
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

boolT = (TyDefType $ boolType basis) []
true  = IdExp boolT IM.empty $ (basisIds basis) ! "True"
false = IdExp boolT IM.empty $ (basisIds basis) ! "False"

-- TODO: take a look at this. should builtins be able to throw errors?
numVal (NumExp _ _ v) = v
numVal e = error $ "Built-in expects a number but got: " ++ (show e)
