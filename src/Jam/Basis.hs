module Jam.Basis (
  Basis (..),
  basis,
  basisProg,
  isTrue,
  isFunctionType
) where

import qualified Util.IndexedMap as IM
import qualified Data.Map as M
import Control.Monad.State
import Jam.Ast
import Jam.Accessors

(basis, basisProg) = runState makeBasis _p
  where _p = Prog { root = BottomExp (TyVarType $ TyVar 0 [RootExpPath]) IM.empty
                  , rootBindings = IM.empty
                  , tydefs = []
                  , tyvarMap = M.empty
                  , uniqC = 1
                  }

data Basis = Basis { basisBindings :: Bindings
                   , basisIds :: M.Map String Id
                   , basisTypes :: [TyDef]
                   , boolType :: TyDef
                   , unitType :: TyDef
                   , numberType :: TyDef
                   , functionType :: TyDef
                   }

isTrue :: Exp -> Bool
isTrue (IdExp _ _ ident) = ident == ((basisIds basis) M.! "True")
isTrue _ = False

isFunctionType :: Type -> Bool
isFunctionType (TyDefType tydef _) = tydef == (functionType basis)
isFunctionType _ = False

makeBasis :: State Prog Basis
makeBasis = do

  -- === TyDefs ===

  -- Function
  u <- getUnique
  let functionType = TyDef u "->" 2 []
      tFun aT rT = TyDefType functionType [aT, rT]

  -- Unit
  u <- getUnique
  let unitType = TyDef u "Unit" 0 []

  -- Number
  u <- getUnique
  let numberType = TyDef u "Number" 0 []
      tNum = TyDefType numberType []

  -- Bool
  [u1, u2, u3] <- getUniques 3
  let boolType = TyDef u1 "Bool" 0 [ TyCon u2 "False" (TyDefType boolType [])
                                   , TyCon u3 "True"  (TyDefType boolType [])
                                   ]
      [cFalse, cTrue] = (constructors boolType)
      tBool = TyDefType boolType []

  -- All types
  let basisTypes = [ functionType
                   , unitType
                   , numberType
                   , boolType
                   ]


  -- === IDs ===
  let tBoolCon   = tBool
      tBinNum    = tFun tNum $ tFun tNum tNum
      tBinNumCmp = tFun tNum $ tFun tNum tBool

  let ids = M.empty
      addId bIds name ty = do u <- getUnique
                              return $ M.insert name (Id u name ty) bIds

  ids <- addId ids "False" tBoolCon
  ids <- addId ids "True"  tBoolCon
  ids <- addId ids "="     tBinNumCmp
  ids <- addId ids "!="    tBinNumCmp
  ids <- addId ids "+"     tBinNum
  ids <- addId ids "-"     tBinNum
  ids <- addId ids "*"     tBinNum
  ids <- addId ids "/"     tBinNum
  ids <- addId ids "%"     tBinNum
  ids <- addId ids "<"     tBinNumCmp
  ids <- addId ids ">"     tBinNumCmp
  ids <- addId ids "<="    tBinNumCmp
  ids <- addId ids ">="    tBinNumCmp

  let i = (M.!) ids
      br  = ExpVal . BuiltInRef
      basisBindings = IM.fromList [ ( i "False", TyConVal cFalse)
                                  , ( i "True" , TyConVal cTrue )
                                  , ( i "="    , br "="          )
                                  , ( i "!="   , br "!="         )
                                  , ( i "+"    , br "+"          )
                                  , ( i "-"    , br "-"          )
                                  , ( i "*"    , br "*"          )
                                  , ( i "/"    , br "/"          )
                                  , ( i "%"    , br "%"          )
                                  , ( i "<"    , br "<"          )
                                  , ( i ">"    , br ">"          )
                                  , ( i "<="   , br "<="         )
                                  , ( i ">="   , br ">="         )
                                  ]


  -- === Prog ===
  p <- get
  put $ p { rootBindings = basisBindings
          , tydefs = basisTypes }


  return $ Basis { basisBindings = basisBindings
                 , basisIds = ids
                 , basisTypes = basisTypes
                 , boolType = boolType
                 , unitType = unitType
                 , numberType = numberType
                 , functionType = functionType
                 }
