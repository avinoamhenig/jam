module Lang.Creators (
  createUnit,
  createNum,
  createLambda,
  createIdExp,
  createApp,
  createIf
) where

import Util.IndexedMap
import Lang.AST

_btm t = BottomExp { bindings = empty, typeof = t }

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
                                 , capturedEnv = empty
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
