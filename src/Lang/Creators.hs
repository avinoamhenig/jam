module Lang.Creators (
  createProg,
  createUnit,
  createNum,
  createLambda,
  createIdExp,
  createApp,
  createIf
) where

import Data.Map
import Lang.AST
import Lang.Basis

_btm t = BottomExp { bindings = empty, typeof = t }

createProg = Prog { root = _btm NoType
                  , rootBindings = basisBindings
                  , tydefs = basisTypes
                  , tyvarMap = empty
                  }

createUnit = UnitExp { bindings = empty
                     , typeof = NoType
                     }

createNum x = NumExp { value = x
                     , bindings = empty
                     , typeof = NoType
                     }

createLambda argName = LambdaExp { argId = Id argName NoType
                                 , body  = _btm NoType
                                 , bindings = empty
                                 , typeof = NoType
                                 }

createIdExp i = IdExp { ident = i
                      , bindings = empty
                      , typeof = NoType
                      }

createApp = AppExp { func = _btm NoType
                   , argVal = _btm NoType
                   , bindings = empty
                   , typeof = NoType
                   }

createIf = IfExp { condExp = _btm NoType
                 , thenExp = _btm NoType
                 , elseExp = _btm NoType
                 , bindings = empty
                 , typeof = NoType
                 }
