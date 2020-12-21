module Day21 where
  import Data.List.Split
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Data.List

  main :: IO ()
  main = do
    allergIngPairs <- map (processIngredients . listToTuple . splitOn "(contains ") . lines <$> readFile "input.txt"
    let possPairs = foldr getPossAllergIngs Map.empty allergIngPairs
    print $ sum (map (Set.size . flip Set.difference (Set.unions $ map snd $ Map.toList possPairs) . fst) allergIngPairs)
    putStrLn $ intercalate "," $ map snd $ Map.toList $ getMatching (Map.toList possPairs)

  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)

  processIngredients :: (String, String) -> (Set String, Set String)
  processIngredients (ings, allergs) = listToTuple $ map Set.fromList [words ings, splitOn ", " $ init allergs]

  getPossAllergIngs :: (Set String, Set String) -> Map String (Set String) -> Map String (Set String)
  getPossAllergIngs (ings, allergs) = Map.unionWith Set.intersection (Map.fromSet (const ings) allergs)

  getMatching :: [(String, Set String)] -> Map String String
  getMatching [] = Map.empty
  getMatching ((allerg, ings):rest) = Map.insert allerg match $ getMatching newList
    where
      match = head $ Set.toList ings
      newList = sortOn (Set.size . snd) $ map (fmap $ Set.delete match) rest
