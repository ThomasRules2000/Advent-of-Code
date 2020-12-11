module Day11 where
  import Data.Matrix (Matrix)
  import qualified Data.Matrix as Matrix
  import Data.Maybe

  main :: IO ()
  main = do
    matrix <- Matrix.fromLists . lines <$> readFile "input.txt"
    print $ gameOfLife 1 1 matrix matrix
    print $ gameOfLife2 1 1 matrix matrix

  gameOfLife :: Int -> Int -> Matrix Char -> Matrix Char -> Int
  gameOfLife r c prev next
    | c > Matrix.ncols prev = if prev == next
        then length $ filter (=='#') $ Matrix.toList next
        else gameOfLife 1 1 next next
    | r > Matrix.nrows prev = gameOfLife 1 (c+1) prev next
    | currChar == 'L' && occupiedAround == 0 = gameOfLife (r+1) c prev $ Matrix.setElem '#' (r,c) next
    | currChar == '#' && occupiedAround >= 4 = gameOfLife (r+1) c prev $ Matrix.setElem 'L' (r,c) next
    | otherwise = gameOfLife (r+1) c prev next
    where
      currChar = Matrix.getElem r c prev
      occupiedAround = getOccupiedAround r c prev

  gameOfLife2 :: Int -> Int -> Matrix Char -> Matrix Char -> Int
  gameOfLife2 r c prev next
    | c > Matrix.ncols prev = if prev == next
        then length $ filter (=='#') $ Matrix.toList next
        else gameOfLife2 1 1 next next
    | r > Matrix.nrows prev = gameOfLife2 1 (c+1) prev next
    | currChar == 'L' && occupiedAround == 0 = gameOfLife2 (r+1) c prev $ Matrix.setElem '#' (r,c) next
    | currChar == '#' && occupiedAround >= 5 = gameOfLife2 (r+1) c prev $ Matrix.setElem 'L' (r,c) next
    | otherwise = gameOfLife2 (r+1) c prev next
    where
      currChar = Matrix.getElem r c prev
      occupiedAround = losOccupied r c prev

  getOccupiedAround :: Int -> Int -> Matrix Char -> Int
  getOccupiedAround r c matrix = sum $ map (fromEnum . (=='#') . fromMaybe ' ' . flip(uncurry Matrix.safeGet) matrix)
    [(r-1,c-1), (r,c-1), (r+1, c-1),
     (r-1,c), (r+1,c),
     (r-1,c+1), (r,c+1), (r+1, c+1)]

  losOccupied :: Int -> Int -> Matrix Char -> Int
  losOccupied r c m = length $ filter id
    [look (r-1) (c-1) (-1) (-1) m, look r (c-1) 0 (-1) m, look (r+1) (c-1) 1 (-1) m,
     look (r-1)  c    (-1)   0  m,                        look (r+1)  c    1   0  m,
     look (r-1) (c+1) (-1)   1  m, look r (c+1) 0   1  m, look (r+1) (c+1) 1   1  m]
    where
      look :: Int -> Int -> Int -> Int -> Matrix Char -> Bool
      look r c nr nc m
        | r > Matrix.nrows m || r < 1 || c > Matrix.ncols m || c < 1 = False
        | currChar == 'L' = False
        | currChar == '#' = True
        | otherwise = look (r+nr) (c+nc) nr nc m
        where currChar = Matrix.getElem r c m
