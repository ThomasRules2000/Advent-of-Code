{-# LANGUAGE ApplicativeDo   #-}
module Day10 where
  import Data.List
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Data.List.Split
  import Data.Maybe

  main :: IO ()
  main = do
    adapters <- sort . map read . lines <$> readFile "input.txt" :: IO [Int]
    let adaptVec = Vec.fromList adapters
    let devJolts = Vec.last adaptVec + 3
    let differences = getDifferences devJolts adaptVec
    print $ (Vec.length $ Vec.filter (==1) differences) * (Vec.length $ Vec.filter (==3) differences)
    print $ dynamicProgramming (Map.singleton devJolts 1) $ reverse (0 : adapters)

  getDifferences :: Int -> Vector Int -> Vector Int
  getDifferences devJolts vec = Vec.zipWith (-) (vec `Vec.snoc` devJolts) (0 `Vec.cons` vec)

  dynamicProgramming :: Map Int Int -> [Int] -> Int
  dynamicProgramming m [] = fromJust $ Map.lookup 0 m
  dynamicProgramming m (x:xs) = dynamicProgramming
                                (Map.insert x
                                  ( (fromMaybe 0 $ Map.lookup (x+1) m)
                                  + (fromMaybe 0 $ Map.lookup (x+2) m)
                                  + (fromMaybe 0 $ Map.lookup (x+3) m))
                                m)
                              xs
