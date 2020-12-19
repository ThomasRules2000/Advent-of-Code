module Day16 where
  import Data.List.Split
  import Data.Ix
  import Data.Set (Set, (\\))
  import qualified Data.Set as Set
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec
  import Field (Field(..))
  import qualified Field
  import Data.Sort
  import Data.List (isPrefixOf)

  main :: IO ()
  main = do
    [f,m,n] <- map lines . splitOn "\n\n" <$> readFile "input.txt" :: IO [[String]]
    let fields = map (processField . listToTuple . splitOn ": ") f
    let myTicket = Vec.fromList $ map read $ splitOn "," $ m !! 1 :: Vector Int
    let nearby = map (map read . splitOn ",") $ tail n :: [[Int]]
    let validVals = Set.unions $ map fieldSet fields
    print $ sumInvalid (concat nearby) validVals

    let validTickets = map Vec.fromList $ filter (all (`Set.member` validVals)) nearby
    print $ Vec.product
          $ Vec.map snd
          $ Vec.filter (isPrefixOf "departure" . fst)
          $ flip Vec.zip myTicket
          $ Vec.update (Vec.replicate (length myTicket) "")
          $ Vec.fromList
          $ fieldToIndex Set.empty
          $ sortOn (Set.size . snd)
          $ getValidFieldNumbers (map (\x -> (x, Set.fromList [0..(length myTicket - 1)])) fields) validTickets

  processField :: (String, String) -> Field
  processField (name, vals) = Field {fieldName=name, fieldSet=Set.unions $ map (Set.fromList . range . listToTuple . map read . splitOn "-") $ splitOn " or " vals}

  getValidFieldNumbers :: [(Field, Set Int)] -> [Vector Int] -> [(Field, Set Int)]
  getValidFieldNumbers fields [] = fields
  getValidFieldNumbers fields (v:vs) = getValidFieldNumbers (map (checkField $ Vec.toList $ Vec.indexed v) fields) vs
    where
      checkField :: [(Int, Int)] -> (Field, Set Int) -> (Field, Set Int)
      checkField [] f = f
      checkField ((i,val):vals) (f, is)
        | val `Set.member` fieldSet f = checkField vals (f, is)
        | otherwise = checkField vals (f, Set.delete i is)

  fieldToIndex :: Set Int -> [(Field, Set Int)] -> [(Int, String)]
  fieldToIndex _ [] = []
  fieldToIndex used ((f,s):fs) = (num, fieldName f):fieldToIndex (Set.insert num used) fs
    where num = head $ Set.toList $ s \\ used

  sumInvalid :: [Int] -> Set Int -> Int
  sumInvalid [] valid = 0
  sumInvalid (x:xs) valid
    | Set.member x valid = next
    | otherwise = x + next
    where next = sumInvalid xs valid

  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)
