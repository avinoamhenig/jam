module Util.IndexedMap (
  Map,
  empty,
  lookup,
  insert,
  foldWithKey,
  fromList,
  (!),
  getMap,
  unionWith
) where

import Prelude hiding (lookup)
import qualified Data.Map

data Map k v = Map { kvMap :: Data.Map.Map k v
                   , ikMap :: Data.Map.Map Int k
                   , kiMap :: Data.Map.Map k Int
                   , _i  :: Int
                   } deriving (Eq)

getMap :: Map k v -> Data.Map.Map k v
getMap = kvMap

empty = Map { kvMap = Data.Map.empty
            , ikMap = Data.Map.empty
            , kiMap = Data.Map.empty
            , _i  = 0
            }

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k im = Data.Map.lookup k $ kvMap im

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v im =
  let newI = if Data.Map.member k $ kvMap im
                  then _i im else (_i im) + 1
      kIndex  = case Data.Map.lookup k $ kiMap im of
                  Nothing -> newI - 1
                  Just i  -> i
  in Map { kvMap = Data.Map.insert k v $ kvMap im
         , ikMap = Data.Map.insert kIndex k $ ikMap im
         , kiMap = Data.Map.insert k kIndex $ kiMap im
         , _i = newI
         }

foldWithKey :: Ord k => (k -> v -> a -> a) -> a -> Map k v -> a
foldWithKey f start im =
  foldr fWrap start $ (Data.Map.keys . ikMap) im
  where fWrap i acc = case Data.Map.lookup i $ ikMap im of
                        Nothing -> acc
                        Just k -> f k ((kvMap im) Data.Map.! k) acc

fromList :: Ord k => [(k, v)] -> Map k v
fromList [] = empty
fromList ((k,v):rest) = insert k v $ fromList rest

im ! k = (kvMap im) Data.Map.! k

unionWith ::  Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith resolve a b = foldWithKey foldFn a b
  where foldFn kb vb a = case lookup kb a of
                          Just va -> insert kb (resolve va vb) a
                          Nothing -> insert kb vb a
