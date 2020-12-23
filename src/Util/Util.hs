module Util.Util where
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  
  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)

  containsKeys :: Ord k => Map k v -> [k] -> Bool
  containsKeys m = all (`Map.member` m)