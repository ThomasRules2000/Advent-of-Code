module Day17Generic where
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Data.Maybe
  import Coord

  main :: IO ()
  main = do
    input <- map Vec.fromList . lines <$> readFile "input.txt"
    print $ Set.size $ doNCycles 6 (getInitialConfig input :: Set Coord3D)
    print $ Set.size $ doNCycles 6 (getInitialConfig input :: Set Coord4D)

  getInitialConfig :: (Coord c) => [Vector Char] -> Set c
  getInitialConfig vs = go vs 0 []
    where
      go :: (Coord c) => [Vector Char] -> Int -> [[Maybe c]] -> Set c
      go [] _ is = Set.fromList $ catMaybes $ concat is
      go (v:vs) n is = go vs (n+1) $ Vec.toList (Vec.imap (getIndex n) v):is
      getIndex :: (Coord c) => Int -> Int -> Char -> Maybe c
      getIndex x y c
        | c == '#' = Just $ from2D (x,y)
        | otherwise = Nothing

  getActiveAround :: (Coord c) => c -> Set c -> Int
  getActiveAround coord set = length $ filter id $ zipWith Set.member iAround $ replicate (length iAround) set
    where iAround = Set.toList $ Set.delete coord $ getNeighbours coord

  processCycle :: (Coord c) => Set c -> Set c
  processCycle set = processNodes toProcess set
    where
      toProcess = Set.toList $ Set.unions $ map getNeighbours $ Set.toList set
      processNodes :: (Coord c) => [c] -> Set c -> Set c
      processNodes [] prev = prev
      processNodes (c:cs) prev
        | Set.member c prev = if numActive /= 2 && numActive /= 3 then Set.delete c next else next
        | otherwise = if numActive == 3 then Set.insert c next else next
        where
          numActive = getActiveAround c prev
          next = processNodes cs prev

  doNCycles :: (Coord c) => Int -> Set c -> Set c
  doNCycles 0 set = set
  doNCycles n set = doNCycles (n-1) $ processCycle set
