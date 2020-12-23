module Day19Alternative where
  import Data.List.Split
  import Data.IntMap.Strict (IntMap)
  import qualified Data.IntMap.Strict as IntMap
  import Data.List (nub)
  import Data.Maybe (fromJust)

  main :: IO ()
  main = do
    [rs, msgs] <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    let rules = IntMap.fromList $ map processRule rs
    print $ length $ filter (doesParse rules) msgs
    print $ length $ filter (doesParse (IntMap.union (IntMap.fromList $ map processRule ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]) rules)) msgs

  processRule :: String -> (Int, Either Char [[Int]])
  processRule s = (read l, getRHS r)
    where
      [l, r] = splitOn ": " s
      getRHS :: String -> Either Char [[Int]]
      getRHS rhs@(r:rs)
        | r == '\"' = Left  $ head rs
        | otherwise = Right $ map read . words <$> splitOn " | " rhs

  doesParse :: IntMap (Either Char [[Int]]) -> String -> Bool
  doesParse rules word = "" `elem` parseWithRule rules [word] 0

  parseWithRule :: IntMap (Either Char [[Int]]) -> [String] -> Int -> [String]
  parseWithRule rMap ss rule = nub $ filter (not . null) ss >>= doParse rMap rule
    where
      doParse :: IntMap (Either Char [[Int]]) -> Int -> String -> [String]
      doParse rMap rule str@(s:ss) = case fromJust $ IntMap.lookup rule rMap of
        Left c -> [ss | s == c]
        Right rss -> rss >>= foldl (parseWithRule rMap) [str]
