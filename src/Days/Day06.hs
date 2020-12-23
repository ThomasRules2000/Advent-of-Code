module Days.Day06 where
  import Data.List.Split
  import Data.IntSet (IntSet)
  import qualified Data.IntSet as IntSet
  import qualified Program.RunDay as R (runDay)

  runDay :: String -> IO ()
  runDay = R.runDay parser part1 part2

  type Input = [[IntSet]]

  type Output1 = Int
  type Output2 = Int

  -- PARSER --
  parser :: String -> Input
  parser = map (map (IntSet.fromList . map fromEnum) . lines) . splitOn "\n\n"

  part1 :: Input -> Output1
  part1 =  sum . map (IntSet.size . IntSet.unions)

  part2 :: Input -> Output2
  part2 = sum . map (IntSet.size . foldl1 IntSet.intersection)
