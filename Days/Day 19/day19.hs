module Day19 where
  import Data.List.Split
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Data.Maybe

  main :: IO ()
  main = do
    [rules, msgs] <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    let validWords = flip getValidWords 0 $ Map.fromList $ map processRule rules
    print $ length $ filter (`Set.member` validWords) msgs
    let validWords2 = getValidWords2 (Map.fromList $ map processRule rules) (maximum $ map length msgs) 0
    print $ filter (`Set.member` validWords2) msgs

  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)

  processRule :: String -> (Int, Either String [[Int]])
  processRule s = (read l, getRHS r)
    where
      [l, r] = splitOn ": " s
      getRHS :: String -> Either String [[Int]]
      getRHS rhs@(r:rs)
        | r == '\"' = Left  $ init rs
        | otherwise = Right $ map (map read . words) $ splitOn " | " rhs

  getValidWords :: Map Int (Either String [[Int]]) -> Int -> Set String
  getValidWords m rule = case fromJust $ Map.lookup rule m of
    Left s -> Set.singleton s
    Right rss -> Set.unions $ map (foldr (appendSets . getValidWords m) (Set.singleton "")) rss

  appendSets :: Set String -> Set String -> Set String
  appendSets a b = Set.map (uncurry (++)) $ Set.cartesianProduct a b

  getValidWords2 :: Map Int (Either String [[Int]]) -> Int -> Int -> Set String
  getValidWords2 m maxWord 8 = rule8 (getValidWords2 m maxWord 42) maxWord
  getValidWords2 m maxWord 11 = rule11 (getValidWords2 m maxWord 42) (getValidWords2 m maxWord 31) maxWord
  getValidWords2 m maxWord rule = case fromJust $ Map.lookup rule m of
    Left s -> Set.singleton s
    Right rss -> Set.unions $ map (foldr (appendSets . getValidWords2 m maxWord) (Set.singleton "")) rss

  rule8 :: Set String -> Int -> Set String
  rule8 s42 n = go s42 $ (n-1 `div` (Set.findMax . Set.map length) s42) + 1
    where
      go :: Set String -> Int -> Set String
      go s 0 = s
      go s n = appendSets (Set.union s $ Set.singleton "") $ go s $ n-1

  rule11 :: Set String -> Set String -> Int -> Set String
  rule11 s42 s31 n = go s42 s31 $ n `div` ((Set.findMax . Set.map length) s42 + (Set.findMax . Set.map length) s31)
    where
      go :: Set String -> Set String -> Int -> Set String
      go s42 s31 0 = appendSets s42 s31
      go s42 s31 n = appendSets s42 $ appendSets (go s42 s31 $ n-1) s31
