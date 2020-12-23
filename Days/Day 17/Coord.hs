module Coord where
  import Data.Set (Set)
  import qualified Data.Set as Set

  class (Ord c) => Coord c where
    getNeighbours :: c -> Set c
    from2D :: (Int, Int) -> c

  newtype Coord3D = Coord3D (Int, Int, Int) deriving (Show, Eq, Ord)
  instance Coord Coord3D where
    getNeighbours (Coord3D (x, y, z)) = Set.fromList [Coord3D (x+x1, y+y1, z+z1) | x1 <- [-1..1], y1 <- [-1..1], z1 <- [-1..1]]
    from2D (x,y) = Coord3D (x, y, 0)

  newtype Coord4D = Coord4D (Int, Int, Int, Int) deriving (Show, Eq, Ord)
  instance Coord Coord4D where
    getNeighbours (Coord4D (w, x, y, z)) = Set.fromList [Coord4D (w+w1, x+x1, y+y1, z+z1) | w1 <- [-1..1], x1 <- [-1..1], y1 <- [-1..1], z1 <- [-1..1]]
    from2D (x,y) = Coord4D (x, y, 0, 0)
