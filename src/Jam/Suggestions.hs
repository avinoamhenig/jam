module Jam.Suggestions (
  suggestions
) where

import Control.Monad.State
import Jts.Ast
import Jam.Ast
import Jam.Accessors
import Jam.Modifiers
import Jam.Creators
import Jam.Basis (basis, functionType)

suggestions :: Prog -> ExpPath -> ExpName -> [[JtsCmd]]
suggestions prog path ename = case expAtPath prog path of
  Nothing -> []
  Just e -> let t = getType e
            in map (toJtsCmds 0 ename) $ findSuggestions prog path t

-- TODO: use uniques to refer to id, not names (requires jts ability
--       to reference identifiers by its unique)
toJtsCmds :: Int -> ExpName -> (Id, Int) -> [JtsCmd]
toJtsCmds _ ename ((Id _ name _), 0) = [RplCmd ename $ CrIdExp (IdName name)]
toJtsCmds c ename (ident, apps) =
  (RplCmd ename $ CrApp (ExpName "_f") (ExpName ("_a" ++ (show apps)))):
    (toJtsCmds (c + 1) (ExpName "_f") (ident, apps - 1))

findSuggestions :: Prog -> ExpPath -> Type -> [(Id, Int)]
findSuggestions prog path t =
  map (\(ident, apps, _) -> (ident, apps)) $
    filter (\(_, _, t') -> evalState (checkType t t') prog)
           (concat $ map (splitApps prog) $ idsVisibleAtPath prog path)

splitApps :: Prog -> Id -> [(Id, Int, Type)]
splitApps p i@(Id _ _ t) = (i, 0, t):(getAppTuples i 1 (finalType p t))

getAppTuples :: Id -> Int -> Type -> [(Id, Int, Type)]
getAppTuples i apps (UniversalType (TyDefType td [_, retT]) i')
  | td == (functionType basis) = (i, apps, retT):
                                  (getAppTuples i (apps + 1)
                                    (UniversalType retT i'))
getAppTuples i apps (TyDefType td [_, retT])
  | td == (functionType basis) = (i, apps, retT):
                                  (getAppTuples i (apps + 1) retT)
getAppTuples _ _ _ = []

checkType :: Type -> Type -> State Prog Bool
checkType et idt = do instT <- instantiateType idt
                      unifies <- unify instT et
                      return unifies
