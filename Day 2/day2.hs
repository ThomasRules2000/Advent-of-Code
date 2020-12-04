module Day2 where
  import Data.List.Split

  main :: IO ()
  main = do
    passwords <- (map words) <$> lines <$> readFile "passwords.txt"
    print $ length $ filter isValid passwords
    print $ length $ filter isValid2 passwords

  isValid :: [String] -> Bool
  isValid [nums, letterColon, pass] = numLetter <= lt && numLetter >= gt
    where
      [gt, lt] = read <$> splitOn "-" nums :: [Int]
      letter = head letterColon
      numLetter = length $ filter (==letter) pass

  isValid2 :: [String] -> Bool
  isValid2 [nums, letterColon, pass] = (pass!!one /= letter) /= (pass!!two /= letter)
    where
      [one, two] = map (subtract 1) (read <$> splitOn "-" nums :: [Int])
      letter = head letterColon
