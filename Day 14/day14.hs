module Day14 where
  import Data.List.Split
  import Data.Bits
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Debug.Trace

  data Instruction = Mem Int Int
                   | Mask String
                   deriving (Eq, Show)

  main :: IO ()
  main = do
    instructions <- processTuples . map (listToTuple . splitOn " = ") . lines <$> readFile "input.txt"
    print $ runInstructions "" Map.empty instructions
    print $ runInstructions2 [] Map.empty instructions

  processTuples :: [(String, String)] -> [Instruction]
  processTuples [] = []
  processTuples ((inst, val):xs)
    | take 3 inst == "mem" = (Mem (read $ init $ drop 4 inst) $ read val):rest
    | otherwise = (Mask val):rest
    where rest = processTuples xs

  runInstructions :: String -> Map Int Int -> [Instruction] -> Int
  runInstructions _ m [] = sum $ map snd $ Map.toList m
  runInstructions mask m (i:is) = case i of
    Mem addr val -> runInstructions mask (Map.insert addr (applyMask mask val) m) is
    Mask newMask -> runInstructions newMask m is

  runInstructions2 :: [String] -> Map Int Int -> [Instruction] -> Int
  runInstructions2 _ m [] = sum $ map snd $ Map.toList m
  runInstructions2 masks m (i:is) = case i of
    Mem addr val -> runInstructions2 masks (Map.union (Map.fromList $ map (\mask -> (applyMask mask addr, val)) masks) m) is
    Mask newMask -> runInstructions2 (allMasks newMask [""]) m is

  applyMask :: String -> Int -> Int
  applyMask mask val = go (reverse mask) val 0
    where
      go :: String -> Int -> Int -> Int
      go [] val _ = val
      go (m:ms) val bitNo
        | m == '0' = go ms (clearBit val bitNo) $ bitNo + 1
        | m == '1' = go ms (setBit val bitNo) $ bitNo + 1
        | otherwise = go ms val $ bitNo + 1

  allMasks :: String -> [String] -> [String]
  allMasks [] masks = masks
  allMasks (m:ms) masks
    | m == 'X' = allMasks ms $ ((++"0") <$> masks) ++ ((++"1") <$> masks)
    | m == '0' = allMasks ms $ (++"X") <$> masks
    | otherwise = allMasks ms $ (++[m]) <$> masks

  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)
