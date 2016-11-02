module Lang.CreateProg (
  createProg
) where

import Util.IndexedMap
import Lang.AST
import Lang.Basis

createProg = Prog { root = BottomExp { bindings = empty, typeof = NoType }
                  , rootBindings = basisBindings
                  , tydefs = basisTypes
                  , tyvarMap = empty
                  }
