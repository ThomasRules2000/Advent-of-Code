{-# LANGUAGE ApplicativeDo   #-}
module Day12 where

  data Action = North Int
              | South Int
              | East Int
              | West Int
              | Rotate Int
              | Forward Int
              deriving (Show, Eq)

  main :: IO ()
  main = do
    actions <- map getActionPairs . lines <$> readFile "input.txt"
    let (x1,y1) = moveShip actions (0,0) 90
    print $ (abs x1) + (abs y1)
    let (x2,y2) = moveShip2 actions (0,0) (10, 1)
    print (x2, y2)
    print $ (abs x2) + (abs y2)

  getActionPairs :: String -> Action
  getActionPairs (a:n) = case a of
    'N' -> North num
    'S' -> South num
    'E' -> East num
    'W' -> West num
    'L' -> Rotate $ negate num
    'R' -> Rotate num
    'F' -> Forward num
    _ -> error "Invalid char"
    where num = read n :: Int

  moveShip :: [Action] -> (Int, Int) -> Int -> (Int, Int)
  moveShip [] pos _ = pos
  moveShip (a:as) (x,y) face = case a of
    North n -> moveShip as (x+n, y) face
    South n -> moveShip as (x-n, y) face
    East n -> moveShip as (x, y+n) face
    West n -> moveShip as (x, y-n) face
    Rotate n -> moveShip as (x, y) (face + n)
    Forward n -> moveShip as (x + n * round (cos rad), y + n * round (sin rad)) face
      where rad = degToRad face

  moveShip2 :: [Action] -> (Int, Int) -> (Int, Int) -> (Int, Int)
  moveShip2 [] pos _  = pos
  moveShip2 (a:as) ship@(sx, sy) way@(wx, wy) = case a of
    North n -> moveShip2 as ship (wx, wy + n)
    South n -> moveShip2 as ship (wx, wy - n)
    East n -> moveShip2 as ship (wx + n, wy)
    West n -> moveShip2 as ship (wx - n, wy)
    Rotate n -> moveShip2 as ship $ rotatePoint way n
    Forward n -> moveShip2 as (sx + n * wx, sy + n * wy) (wx, wy)


  degToRad :: Int -> Float
  degToRad deg = fromIntegral deg * (pi/180)

  rotatePoint :: (Int, Int) -> Int -> (Int, Int)
  rotatePoint (x, y) deg = (x*c + y*s, y*c - x*s)
    where
      rad = degToRad deg
      s = round $ sin rad
      c = round $ cos rad
