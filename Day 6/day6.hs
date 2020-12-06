{-# LANGUAGE ApplicativeDo   #-}
module Day6 where
  import Data.List.Split
  import qualified Data.Set as Set

  main :: IO ()
  main = do
    answers <- map (map Set.fromList . lines) . splitOn "\n\n" <$> readFile "input.txt"
    print $ sum $ map (Set.size . Set.unions) answers
    print $ sum $ map (Set.size . intersections) answers


  intersections :: (Foldable t, Ord a) => t (Set.Set a) -> Set.Set a
  intersections = foldl1 Set.intersection
