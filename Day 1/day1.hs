{-# LANGUAGE ApplicativeDo   #-}
module Day1 where

  main :: IO ()
  main = do
    nums <- map read . lines <$> readFile "input.txt" :: IO [Int]
    print $ head [x*y | x <- nums, y <- nums, x+y==2020]
    print $ head [x*y*z | x <- nums, y <- nums, z <- nums, x+y+z==2020]
