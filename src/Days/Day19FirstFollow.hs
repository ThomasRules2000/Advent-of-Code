module Day19New where
  import Data.List.Split
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Data.Maybe
  import Data.Either
  import Debug.Trace

  main :: IO ()
  main = do
    [rs, msgs] <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    let rules = Map.fromList $ map processRule rs
    let sorted = tsort rules
    print sorted
    let firstSets = calcFirstSets (reverse sorted) rules Map.empty
    print firstSets
    print $ calcFollowSets (tail sorted) rules firstSets $ Map.singleton 0 $ Set.singleton '$'

  listToTuple :: Show a => [a] -> (a,a)
  listToTuple [x,y] = (x,y)
  listToTuple xs = error $ show xs

  processRule :: String -> (Int, Either Char [[Int]])
  processRule s = (read l, getRHS r)
    where
      [l, r] = splitOn ": " s
      getRHS :: String -> Either Char [[Int]]
      getRHS rhs@(r:rs)
        | r == '\"' = Left  $ head rs
        | otherwise = Right $ map (map read . words) $ splitOn " | " rhs

  tsort :: Map Int (Either Char [[Int]]) -> [Int]
  tsort m = tail $ reverse $ go [] (Map.keys $ Map.filter (==0) numIncoming) edgeMap numIncoming
    where
      edgeMap = Map.map (concat . fromRight [[]]) m
      numIncoming = getNumIncoming (Map.toList edgeMap) Map.empty
      go :: [Int] -> [Int] -> Map Int [Int] -> Map Int Int -> [Int]
      go sorted [] _ _ = sorted
      go sorted (n:noIn) edgeMap numIncomingMap = go (n:sorted) newNoIn edgeMap $ Map.filter (>0) newIncoming
        where
          ms = fromJust $ Map.lookup n edgeMap
          newIncoming = foldr (Map.adjust (subtract 1)) numIncomingMap ms
          newNoIn = noIn ++ Map.keys (Map.filter (==0) newIncoming)

  getNumIncoming :: [(Int, [Int])] -> Map Int Int -> Map Int Int
  getNumIncoming [] m = m
  getNumIncoming ((rule, edges):rest) m = getNumIncoming rest $ foldr addOne (Map.insertWith (flip const) rule 0 m) edges
    where
      addOne :: Int -> Map Int Int -> Map Int Int
      addOne rule m
        | Map.member rule m = Map.adjust (+1) rule m
        | otherwise = Map.insert rule 1 m


  calcFirstSets :: [Int] -> Map Int (Either Char [[Int]]) -> Map Int (Set Char) -> Map Int (Set Char)
  calcFirstSets [] _ firstSets = firstSets
  calcFirstSets (r:rs) ruleMap firstSets = case fromJust $ Map.lookup r ruleMap of
    Left c -> calcFirstSets rs ruleMap $ Map.insert r (Set.singleton c) firstSets
    Right rss -> calcFirstSets rs ruleMap $ Map.insert r (Set.unions $ map (fromJust . flip Map.lookup firstSets . head) rss) firstSets

  calcFollowSets :: [Int] -> Map Int (Either Char [[Int]]) -> Map Int (Set Char) -> Map Int (Set Char) -> Map Int (Set Char)
  calcFollowSets [] _ _ followSets = followSets
  calcFollowSets (r:rs) ruleMap firstSets followSets = calcFollowSets rs ruleMap firstSets
                                                       $ Map.insert r (Set.unions $ map (getFollow r)
                                                                                  $ Map.toList
                                                                                  $ Map.map (fromRight [])
                                                                                  $ Map.filter (any (elem r) . fromRight []) ruleMap)
                                                                      followSets
    where
      getFollow :: Int -> (Int, [[Int]]) -> Set Char
      getFollow curr (r, rss) = Set.unions $ map (getFollow' curr r) rss
      getFollow' :: Int -> Int -> [Int] -> Set Char
      getFollow' curr r [x] = fromJust $ Map.lookup r followSets
      getFollow' curr r [one, two]
        | one == curr = fromJust $ Map.lookup two firstSets
        | otherwise = fromJust $ Map.lookup r followSets


  wordInLang :: Map Int (Either Char [[Int]]) -> Map Int (Set Char) -> Map Int (Set Char) String -> Bool
  wordInLang rules firstSets followSets word = False
