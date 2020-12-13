module Day13 where
  import Data.List.Split
  import Data.List
  import Data.Maybe
  import Text.Read

-- (x1,t1), (x2,t2), (x3,t3)
-- t0 % x1 == t0+t1 % x2 == t0+t1+t2 % x3

  main :: IO ()
  main = do
    [s, b] <- lines <$> readFile "input.txt"
    let buses = fromMaybe 0 . readMaybe <$> splitOn "," b :: [Int]
    print $ waitTime (read s) 0 $ filter (/=0) buses
    let busTuples = tail $ count0s buses 0 0
    print $ consecDeps 0 1 0 busTuples busTuples

  waitTime :: Int -> Int -> [Int] -> Int
  waitTime time waited buses = case checkBuses time buses of
    Nothing -> waitTime (time+1) (waited+1) buses
    Just bus -> waited*bus
    where
      checkBuses :: Int -> [Int] -> Maybe Int
      checkBuses _ [] = Nothing
      checkBuses time (b:bs)
        | time `mod` b == 0 = Just b
        | otherwise = checkBuses time bs

  count0s :: [Int] -> Int -> Int -> [(Int, Int)]
  count0s [] n c = [(n,c)]
  count0s (x:xs) n c
    | x == 0 = count0s xs n $ c+1
    | otherwise = (n,c):count0s xs x 1

  consecDeps :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -> Int
  consecDeps start _ _ [] _ = start
  consecDeps start step time ((b,n):bs) allBuses
    | time `mod` b == 0 = consecDeps start (lcm step b) (time+n) bs allBuses
    | otherwise = consecDeps newStart step newStart allBuses allBuses
    where newStart = start + step
