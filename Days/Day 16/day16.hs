module Day16 where
  import Data.List.Split
  import Data.Ix (range)
  import Data.IntSet (IntSet)
  import qualified Data.IntSet as IntSet
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec
  import Field (Field(..))
  import qualified Field
  import Data.List (isPrefixOf, sortOn)

  main :: IO ()
  main = do
    [f,m,n] <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    let fields = map (processField . listToTuple . splitOn ": ") f
    let myTicket = Vec.fromList $ processTicket $ last m
    let nearby = map processTicket $ tail n
    let validVals = IntSet.unions $ map fieldSet fields
    print $ sumInvalid (concat nearby) validVals
    print $ Vec.product
          $ Vec.map snd
          $ Vec.filter (isPrefixOf "departure" . fst)
          $ flip Vec.zip myTicket
          $ Vec.update (Vec.replicate (length myTicket) "")
          $ Vec.fromList
          $ fieldToIndex IntSet.empty
          $ sortOn (IntSet.size . snd)
          $ getValidFieldNumbers (map (\x -> (x, IntSet.fromList [0..(length myTicket - 1)])) fields)
          $ map Vec.fromList
          $ filter (all (`IntSet.member` validVals)) nearby

  processField :: (String, String) -> Field
  processField (name, vals) = Field {fieldName=name, fieldSet=IntSet.unions $ map (IntSet.fromList . range . listToTuple . map read . splitOn "-") $ splitOn " or " vals}

  processTicket :: String -> [Int]
  processTicket = map read . splitOn ","

  getValidFieldNumbers :: [(Field, IntSet)] -> [Vector Int] -> [(Field, IntSet)]
  getValidFieldNumbers fields [] = fields
  getValidFieldNumbers fields (v:vs) = getValidFieldNumbers (map (checkField $ Vec.toList $ Vec.indexed v) fields) vs
    where
      checkField :: [(Int, Int)] -> (Field, IntSet) -> (Field, IntSet)
      checkField [] f = f
      checkField ((i,val):vals) (f, is)
        | val `IntSet.member` fieldSet f = checkField vals (f, is)
        | otherwise = checkField vals (f, IntSet.delete i is)

  fieldToIndex :: IntSet -> [(Field, IntSet)] -> [(Int, String)]
  fieldToIndex _ [] = []
  fieldToIndex used ((f,s):fs) = (num, fieldName f):fieldToIndex (IntSet.insert num used) fs
    where num = head $ IntSet.toList $ s IntSet.\\ used

  sumInvalid :: [Int] -> IntSet -> Int
  sumInvalid [] valid = 0
  sumInvalid (x:xs) valid
    | IntSet.member x valid = next
    | otherwise = x + next
    where next = sumInvalid xs valid

  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)
