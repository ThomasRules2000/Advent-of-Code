module Day15 where
  import Data.IntMap.Strict (IntMap)
  import qualified Data.IntMap.Strict as IntMap
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec
  import Data.Maybe
  import Debug.Trace
  import Data.Tuple

  --startingNumbers = map (fmap (+1) . swap) $ Vec.toList $ Vec.indexed $ Vec.fromList [11,0,1,10,5,19]
  --startingNumbers = [(3,1),(1,2),(2,3)]
  startingNumbers = [(11,1), (0,2), (1,3), (10,4), (5,5), (19,6)]


  main :: IO ()
  main = print $ doTurn (length startingNumbers+1) (fst $ last startingNumbers) (IntMap.fromList $ init startingNumbers)

  doTurn :: Int -> Int -> IntMap Int -> Int
  doTurn 30000001 n _ = n
  doTurn turnNo prevNo m = doTurn (turnNo + 1) nextNo $! IntMap.insert prevNo (turnNo-1) m
    where
      nextNo = case IntMap.lookup prevNo m of
        Nothing -> 0
        Just prevTurn -> turnNo - prevTurn - 1
