{-# LANGUAGE ApplicativeDo   #-}
module Day5 where
  import Data.List

  main :: IO ()
  main = do
    seats <- map (foldl (\x y -> 2*x + fromEnum (y=='B' || y=='R')) 0) . lines <$> readFile "input.txt"
    print $ maximum seats
    let sorted = sort seats
    print $ fst $ head $ filter (uncurry (/=)) $ zip [head sorted..] sorted
