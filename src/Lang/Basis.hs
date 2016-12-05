module Lang.Basis (
  basisProg,
  basis,
  basisBindings,
  basisIds,
  basisTypes,
  boolType,
  cTrue,
  cFalse,
  unitType,
  numberType,
  functionType
) where

import qualified Util.IndexedMap as IM
import qualified Data.Map as M
import Control.Monad.State
import Lang.AST
import Lang.Accessors

basisProg     = snd basisAndProg
basis         = fst basisAndProg
basisBindings = _basisBindings basis
basisIds      = _basisIds      basis
basisTypes    = _basisTypes    basis
boolType      = _boolType      basis
cTrue         = _cTrue         basis
cFalse        = _cFalse        basis
unitType      = _unitType      basis
numberType    = _numberType    basis
functionType  = _functionType  basis

basisAndProg = runState makeBasis _p
  where _p = Prog { root = BottomExp { bindings = IM.empty,
                                       typeof = TyVarType $ TyVar 0 }
                  , rootBindings = IM.empty
                  , tydefs = []
                  , tyvarMap = M.empty
                  , uniqC = 1
                  }

data Basis = Basis { _basisBindings :: Bindings
                   , _basisIds :: M.Map String Id
                   , _basisTypes :: [TyDef]
                   , _boolType :: TyDef
                   , _cTrue :: TyCon
                   , _cFalse :: TyCon
                   , _unitType :: TyDef
                   , _numberType :: TyDef
                   , _functionType :: TyDef
                   }

makeBasis :: State Prog Basis
makeBasis = do
  let wU f = getUnique >>= \u -> return $ f u

  -- TyDefs
  _functionType <- wU (\u -> TyDef u "->"     2 [])
  _unitType     <- wU (\u -> TyDef u "Unit"   0 [])
  _numberType   <- wU (\u -> TyDef u "Number" 0 [])

  let tFun aT rT = TyDefType _functionType [aT, rT]
      tUnit      = TyDefType _unitType []
      tNum       = TyDefType _numberType []

  u1 <- getUnique
  u2 <- getUnique
  u3 <- getUnique
  let _boolType = TyDef u1 "Bool" 0 [
                    TyCon u2 "False" (tFun tUnit (TyDefType _boolType [])),
                    TyCon u3 "True"  (tFun tUnit (TyDefType _boolType []))
                  ]
      [_cFalse, _cTrue] = (constructors _boolType)
      tBool = TyDefType _boolType []

  let _basisTypes = [ _functionType
                   , _unitType
                   , _numberType
                   , _boolType
                   ]

  -- IDs
  let _basisIds = M.empty
      addId bIds name ty = wU $ \u -> M.insert name (Id u name ty) bIds

  let tBoolCon   = tFun tUnit tBool
      tBinNum    = tFun tNum $ tFun tNum tNum
      tBinNumCmp = tFun tNum $ tFun tNum tBool

  _basisIds <- addId _basisIds "False" tBoolCon
  _basisIds <- addId _basisIds "True"  tBoolCon
  _basisIds <- addId _basisIds "="     tBinNumCmp
  _basisIds <- addId _basisIds "+"     tBinNum
  _basisIds <- addId _basisIds "-"     tBinNum
  _basisIds <- addId _basisIds "*"     tBinNum
  _basisIds <- addId _basisIds "/"     tBinNum
  _basisIds <- addId _basisIds "%"     tBinNum
  _basisIds <- addId _basisIds "<"     tBinNumCmp

  let (!) = (M.!)
      br  = ExpVal . BuiltInRef
  let _basisBindings = IM.fromList [ ( _basisIds ! "False", TyConVal _cFalse)
                                   , ( _basisIds ! "True" , TyConVal _cTrue )
                                   , ( _basisIds ! "="    , br "="          )
                                   , ( _basisIds ! "+"    , br "+"          )
                                   , ( _basisIds ! "-"    , br "-"          )
                                   , ( _basisIds ! "*"    , br "*"          )
                                   , ( _basisIds ! "/"    , br "/"          )
                                   , ( _basisIds ! "%"    , br "%"          )
                                   , ( _basisIds ! "<"    , br "<"          )
                                   ]

  -- update prog
  p <- get
  put $ p { rootBindings = _basisBindings
          , tydefs = _basisTypes }

  -- return
  return $ Basis { _basisBindings = _basisBindings
                 , _basisIds = _basisIds
                 , _basisTypes = _basisTypes
                 , _boolType = _boolType
                 , _cTrue = _cTrue
                 , _cFalse = _cFalse
                 , _unitType = _unitType
                 , _numberType = _numberType
                 , _functionType = _functionType
                 }
