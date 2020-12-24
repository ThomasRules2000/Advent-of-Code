module Days.Day17OLD where
import           Data.Maybe
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

type Coord3D = (Int,Int,Int)
type ActivitySet3D = Set Coord3D

type Coord4D = (Int,Int,Int,Int)
type ActivitySet4D = Set Coord4D

main :: IO ()
main = do
  input <- map Vec.fromList . lines <$> readFile "input.txt"
  print $ Set.size $ doNCycles3D 6 $ getInitialConfig3D input 0 []
  print $ Set.size $ doNCycles4D 6 $ getInitialConfig4D input 0 []

getInitialConfig3D :: [Vector Char] -> Int -> [[Maybe Coord3D]] -> ActivitySet3D
getInitialConfig3D [] _ is = Set.fromList $ catMaybes $ concat is
getInitialConfig3D (v:vs) n is = getInitialConfig3D vs (n+1) $ (Vec.toList (Vec.imap (getIndex n) v)):is
  where
    getIndex :: Int -> Int -> Char -> Maybe Coord3D
    getIndex x y c
      | c == '#' = Just (x,y,0)
      | otherwise = Nothing

getIndexesAround3D :: Coord3D -> Set Coord3D
getIndexesAround3D (x,y,z) = Set.fromList [(x+x1,y+y1,z+z1) | x1 <- [-1..1], y1 <- [-1..1], z1 <- [-1..1]]

getActiveAround3D :: Coord3D -> ActivitySet3D -> Int
getActiveAround3D (x,y,z) set = length $ filter id $ zipWith Set.member iAround $ replicate (length iAround) set
  where iAround = Set.toList $ Set.delete (x,y,z) $ getIndexesAround3D (x,y,z)

processCycle3D :: ActivitySet3D -> ActivitySet3D
processCycle3D set = processNodes toProcess set
  where
    toProcess = Set.toList $ Set.unions $ map getIndexesAround3D $ Set.toList set
    processNodes :: [Coord3D] -> ActivitySet3D -> ActivitySet3D
    processNodes [] prev = prev
    processNodes (c:cs) prev
      | Set.member c prev = if numActive /= 2 && numActive /= 3 then Set.delete c next else next
      | otherwise = if numActive == 3 then Set.insert c next else next
      where
        numActive = getActiveAround3D c prev
        next = processNodes cs prev

doNCycles3D :: Int -> ActivitySet3D -> ActivitySet3D
doNCycles3D 0 set = set
doNCycles3D n set = doNCycles3D (n-1) $ processCycle3D set

------

getInitialConfig4D :: [Vector Char] -> Int -> [[Maybe Coord4D]] -> ActivitySet4D
getInitialConfig4D [] _ is = Set.fromList $ catMaybes $ concat is
getInitialConfig4D (v:vs) n is = getInitialConfig4D vs (n+1) $ (Vec.toList (Vec.imap (getIndex n) v)):is
  where
    getIndex :: Int -> Int -> Char -> Maybe Coord4D
    getIndex x y c
      | c == '#' = Just (x,y,0,0)
      | otherwise = Nothing

getIndexesAround4D :: Coord4D -> Set Coord4D
getIndexesAround4D (w,x,y,z) = Set.fromList [(w+w1,x+x1,y+y1,z+z1) | w1<- [-1..1], x1 <- [-1..1], y1 <- [-1..1], z1 <- [-1..1]]

getActiveAround4D :: Coord4D -> ActivitySet4D -> Int
getActiveAround4D (w,x,y,z) set = length $ filter id $ zipWith Set.member iAround $ replicate (length iAround) set
  where iAround = Set.toList $ Set.delete (w,x,y,z) $ getIndexesAround4D (w,x,y,z)

processCycle4D :: ActivitySet4D -> ActivitySet4D
processCycle4D set = processNodes toProcess set
  where
    toProcess = Set.toList $ Set.unions $ map getIndexesAround4D $ Set.toList set
    processNodes :: [Coord4D] -> ActivitySet4D -> ActivitySet4D
    processNodes [] prev = prev
    processNodes (c:cs) prev
      | Set.member c prev = if numActive /= 2 && numActive /= 3 then Set.delete c next else next
      | otherwise = if numActive == 3 then Set.insert c next else next
      where
        numActive = getActiveAround4D c prev
        next = processNodes cs prev

doNCycles4D :: Int -> ActivitySet4D -> ActivitySet4D
doNCycles4D 0 set = set
doNCycles4D n set = doNCycles4D (n-1) $ processCycle4D set
