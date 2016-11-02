module Util.IndexedMap (
  Map,
  empty,
  lookup,
  insert,
  foldWithKey,
  fromList,
  (!),
  size
) where

import Prelude hiding (lookup)
import qualified Data.Map
import Data.List (foldl')

data Map k v = Map { kvMap :: Data.Map.Map k v
                   , ikMap :: Data.Map.Map Integer k
                   , kiMap :: Data.Map.Map k Integer
                   , size  :: Integer
                   } deriving (Eq)

empty = Map { kvMap = Data.Map.empty
            , ikMap = Data.Map.empty
            , kiMap = Data.Map.empty
            , size  = 0
            }

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k im = Data.Map.lookup k $ kvMap im

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v im =
  let newSize = if Data.Map.member k $ kvMap im
                  then size im else (size im) + 1
      kIndex  = case Data.Map.lookup k $ kiMap im of
                  Nothing -> newSize - 1
                  Just i  -> i
  in Map { kvMap = Data.Map.insert k v $ kvMap im
         , ikMap = Data.Map.insert kIndex k $ ikMap im
         , kiMap = Data.Map.insert k kIndex $ kiMap im
         , size = newSize
         }

foldWithKey :: Ord k => (k -> v -> a -> a) -> a -> Map k v -> a
foldWithKey f start im = foldl' fWrap start [0..(size im)-1]
  where fWrap acc i = case Data.Map.lookup i $ ikMap im of
                        Nothing -> acc
                        Just k -> f k ((kvMap im) Data.Map.! k) acc

fromList :: Ord k => [(k, v)] -> Map k v
fromList [] = empty
fromList ((k,v):rest) = insert k v $ fromList rest

im ! k = (kvMap im) Data.Map.! k
