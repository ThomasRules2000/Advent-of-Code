{-# LANGUAGE ApplicativeDo   #-}
module Day9 where
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec

  main :: IO ()
  main = do
    nums <- map read . lines <$> readFile "input.txt" :: IO [Int]
    let invalid = findInvalid (drop 25 nums) (Vec.fromListN 25 nums)
    print invalid
    print $ findWeakness invalid nums

  isValid :: Int -> Vector Int -> Bool
  isValid n vec = n `elem` [x+y | x <- Vec.toList vec, y <- Vec.toList vec]

  findInvalid :: [Int] -> Vector Int -> Int
  findInvalid (x:xs) vec
    | isValid x vec = findInvalid xs $ Vec.tail vec `Vec.snoc` x
    | otherwise = x

  findWeakness :: Int -> [Int] -> Int
    findWeakness n xs = case Vec.findIndex (==n) $ Vec.imap sumN $ Vec.replicate (length xs - 1) xs of
    Nothing -> findWeakness n $ tail xs
    Just i -> maximum range + minimum range
      where range = take i xs

  sumN :: Num a => Int -> [a] -> a
  sumN n xs = sum $ take (n+2) xs
