{-# LANGUAGE ApplicativeDo   #-}
module Day5 where
  import Data.List

  main :: IO ()
  main = do
    seats <- map getSeatNum . lines <$> readFile "input.txt"
    print $ maximum seats
    let (s:sorted) = sort seats
    print $ findMissing s sorted


  getSeatNum :: String -> Int
  getSeatNum s = getSeatNum' $ reverse s
    where
      getSeatNum' [] = 0
      getSeatNum' (x:xs) = case x of
        'F' -> nextSeat
        'B' -> 1 + nextSeat
        'L' -> nextSeat
        'R' -> 1 + nextSeat
        _   -> error "Invalid Seat"
        where nextSeat = 2 * getSeatNum' xs

  findMissing :: Int -> [Int] -> Int
  findMissing _ [] = error "Not Found"
  findMissing prev (s:ss)
    | s /= prev + 1 = prev + 1
    | otherwise = findMissing s ss
