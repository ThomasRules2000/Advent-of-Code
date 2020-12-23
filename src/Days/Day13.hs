{-# LANGUAGE ApplicativeDo   #-}
module Day13 where
  import Data.List.Split
  import Data.Maybe
  import Text.Read (readMaybe)

  main :: IO ()
  main = do
    [s, b] <- lines <$> readFile "input.txt"
    let buses = readMaybe <$> splitOn "," b :: [Maybe Int]
    print $ waitTime (read s) 0 $ catMaybes buses
    let busTuples = tail $ countNothings buses 0 0
    print $ consecDeps 0 1 0 busTuples busTuples

  waitTime :: Int -> Int -> [Int] -> Int
  waitTime time waited buses
    | null arriving = waitTime (time + 1) (waited + 1) buses
    | otherwise = waited * head arriving
    where arriving = filter (\b -> time `mod` b == 0) buses

  countNothings :: [Maybe Int] -> Int -> Int -> [(Int, Int)]
  countNothings [] n c = [(n,c)]
  countNothings (x:xs) n c = case x of
    Nothing -> countNothings xs n $ c+1
    Just y -> (n,c):countNothings xs y 1

  consecDeps :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -> Int
  consecDeps start _ _ [] _ = start
  consecDeps start step time ((b,n):bs) allBuses
    | time `mod` b == 0 = consecDeps start (lcm step b) (time+n) bs allBuses
    | otherwise = consecDeps newStart step newStart allBuses allBuses
    where newStart = start + step
