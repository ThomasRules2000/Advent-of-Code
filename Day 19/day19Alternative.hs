module Day19Alternative where
  import Data.List.Split
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Data.Either
  import Data.List (nub)
  import Data.Maybe

  main :: IO ()
  main = do
    [rs, msgs] <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    let rules = Map.fromList $ map processRule rs
    print $ length $ filter (doesParse rules) msgs
    print $ length $ filter (doesParse (Map.union (Map.fromList $ map processRule ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]) rules)) msgs

  processRule :: String -> (Int, Either Char [[Int]])
  processRule s = (read l, getRHS r)
    where
      [l, r] = splitOn ": " s
      getRHS :: String -> Either Char [[Int]]
      getRHS rhs@(r:rs)
        | r == '\"' = Left  $ head rs
        | otherwise = Right $ map (map read . words) $ splitOn " | " rhs

  doesParse :: Map Int (Either Char [[Int]]) -> String -> Bool
  doesParse rules word = "" `elem` parseWithRule rules 0 [word]

  parseWithRule :: Map Int (Either Char [[Int]]) -> Int -> [String] -> [String]
  parseWithRule rMap rule ss = nub $ concatMap (doParse rMap rule) $ filter (not . null) ss
    where
      doParse :: Map Int (Either Char [[Int]]) -> Int -> String -> [String]
      doParse rMap rule str@(s:ss) = case fromJust $ Map.lookup rule rMap of
        Left c -> if s == c then [ss] else []
        Right rss -> concatMap (foldl (flip $ parseWithRule rMap) [str]) rss
