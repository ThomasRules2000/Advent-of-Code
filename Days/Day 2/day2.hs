{-# LANGUAGE ApplicativeDo   #-}
module Day2 where
  import Data.List.Split

  main :: IO ()
  main = do
    passwords <- map (processPasswords . words) . lines <$> readFile "input.txt"
    print $ length $ filter isValid passwords
    print $ length $ filter isValid2 passwords

  processPasswords :: [String] -> (Int, Int, Char, String)
  processPasswords [nums, letterColon, pass] = (x, y, head letterColon, pass)
    where [x,y] = read <$> splitOn "-" nums :: [Int]

  isValid :: (Int, Int, Char, String) -> Bool
  isValid (gt, lt, letter, pass) = numLetter <= lt && numLetter >= gt
    where numLetter = length $ filter (==letter) pass

  isValid2 :: (Int, Int, Char, String) -> Bool
  isValid2 (one, two, letter, pass) = (pass!!(one-1) /= letter) /= (pass!!(two-1) /= letter)
