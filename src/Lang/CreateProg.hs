module Lang.CreateProg (
  createProg
) where

import qualified Util.IndexedMap
import Data.Map
import Lang.AST
import Lang.Basis

createProg = Prog { root = BottomExp { bindings = Util.IndexedMap.empty,
                                      typeof = TyVarType $ TyVar 0 }
                 , rootBindings = basisBindings
                 , tydefs = basisTypes
                 , tyvarMap = empty
                 , uniqC = 1
                 }
