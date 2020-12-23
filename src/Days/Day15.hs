module Day15 where
  import Data.IntMap.Strict (IntMap)
  import qualified Data.IntMap.Strict as IntMap

  startingNumbers :: [Int]
  startingNumbers = [11,0,1,10,5,19]

  main :: IO ()
  main = do
    let lastOccurences = IntMap.fromList $ init $ zip startingNumbers [0..]
    let startTurn = length startingNumbers
    let startNo = last startingNumbers
    print $ doTurn 2020 startTurn startNo lastOccurences
    print $ doTurn 30000000 startTurn startNo lastOccurences

  doTurn :: Int -> Int -> Int -> IntMap Int -> Int
  doTurn endTurn turnNo prevNo m
    | turnNo == endTurn = prevNo
    | otherwise = doTurn endTurn (turnNo + 1) nextNo $! IntMap.insert prevNo (turnNo-1) m
    where
      nextNo = case IntMap.lookup prevNo m of
        Nothing -> 0
        Just prevTurn -> turnNo - prevTurn - 1
