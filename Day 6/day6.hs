{-# LANGUAGE ApplicativeDo   #-}
module Day6 where
  import Data.List.Split
  import qualified Data.IntSet as IntSet

  main :: IO ()
  main = do
    answers <- map (map (IntSet.fromList . map fromEnum) . lines) . splitOn "\n\n" <$> readFile "input.txt"
    print $ sum $ map (IntSet.size . IntSet.unions) answers
    print $ sum $ map (IntSet.size . foldl1 IntSet.intersection) answers
