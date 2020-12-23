{-# LANGUAGE ApplicativeDo   #-}
module Day3 where
  import Data.Matrix (Matrix)
  import qualified Data.Matrix as Matrix

  main :: IO ()
  main = do
    slope <- Matrix.fromLists . map (map (=='#')) . lines <$> readFile "input.txt"
    print $ countTrees slope (1,1) (3,1)
    print $ product $ map (countTrees slope (1,1)) [(1,1), (3,1), (5,1), (7,1), (1,2)]

  countTrees :: Matrix Bool -> (Int,Int) -> (Int,Int) -> Int
  countTrees slope (x, y) dir@(xDir, yDir)
    | y > Matrix.nrows slope = 0
    | Matrix.getElem y x slope = 1 + next
    | otherwise = next
    where
      newX = ((x + xDir - 1) `mod` Matrix.ncols slope) + 1
      next = countTrees slope (newX, y+yDir) dir
