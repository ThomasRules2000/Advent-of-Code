module Days.Day15 where
  import Data.IntMap.Strict (IntMap)
  import qualified Data.IntMap.Strict as IntMap
  import Data.Tuple.Extra
  import Data.List.Split
  import qualified Program.RunDay as R (runDay)

  runDay :: String -> IO ()
  runDay = R.runDay parser part1 part2

  type Input = (Int, Int, IntMap Int)

  type Output1 = Int
  type Output2 = Int

  parser :: String -> Input
  parser s = (length startingNos, last startingNos, IntMap.fromList $ init $ zip startingNos [0..])
    where startingNos = read <$> splitOn "," s :: [Int]

  part1 :: Input -> Output1
  part1 = uncurry3 (doTurn 2020)

  part2 :: Input -> Output2
  part2 = uncurry3 (doTurn 30000000)

  doTurn :: Int -> Int -> Int -> IntMap Int -> Int
  doTurn endTurn turnNo prevNo m
    | turnNo == endTurn = prevNo
    | otherwise = doTurn endTurn (turnNo + 1) nextNo $! IntMap.insert prevNo (turnNo-1) m
    where
      nextNo = case IntMap.lookup prevNo m of
        Nothing -> 0
        Just prevTurn -> turnNo - prevTurn - 1
