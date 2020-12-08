{-# LANGUAGE ApplicativeDo   #-}
module Day7 where
  import Data.List.Split
  import Data.Char
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import Data.List
  import Data.Maybe

  main :: IO ()
  main = do
    bags <- processTuples . map (listToTuple . splitOn " contain ") . lines <$> readFile "input.txt"
    let bagMap = Map.fromList bags
    print $ getNumAncestors bags bags ["shiny gold"] (Set.singleton "shiny gold")
    print $ getNumDecendants "shiny gold" bagMap

  listToTuple :: [a] -> (a, a)
  listToTuple [x,y] = (x,y)

  processTuples :: [(String, String)] -> [(String, Map.Map String Int)]
  processTuples [] = []
  processTuples ((k,v):rest) = (removeBag k, Map.fromList $ getBagTuples v):processTuples rest

  getBagTuples :: String -> [(String, Int)]
  getBagTuples "no other bags." = []
  getBagTuples s = (map getTuple . splitOn ", " . init) s
    where
      getTuple :: String -> (String, Int)
      getTuple (num:space:bag) = (removeBag bag, digitToInt num)

  removeBag :: String -> String
  removeBag = unwords . init . words

  getNumAncestors :: [(String, Map.Map String Int)] -> [(String, Map.Map String Int)] -> [String] -> Set.Set String -> Int
  getNumAncestors _ _ [] ancestors = Set.size ancestors - 1
  getNumAncestors [] full (y:ys) added = getNumAncestors full full ys added
  getNumAncestors ((bag, conts):xs) full curr added
    | isJust $ Map.lookup (head curr) conts = if Set.member bag added
                                    then getNumAncestors xs full curr added
                                    else getNumAncestors xs full (curr ++ [bag]) (Set.insert bag added)
    | otherwise = getNumAncestors xs full curr added

  getNumDecendants :: String -> Map.Map String (Map.Map String Int) -> Int
  getNumDecendants bag m = sum $ map (\(newBag, num) -> num * (1 + getNumDecendants newBag m)) $ Map.toList $ fromJust $ Map.lookup bag m
