{-# LANGUAGE ApplicativeDo   #-}
module Day8 where
  import Data.Vector (Vector, (!), (//))
  import qualified Data.Vector as V

  import Data.Set (Set)
  import qualified Data.Set as S

  main :: IO ()
  main = do
    prog <- V.fromList . map (sndToInt . listToTuple . words) . lines <$> readFile "input.txt"
    print $ fst $ runProg 0 0 prog S.empty
    print $ findCorrect 0 prog

  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)

  sndToInt :: (a, String) -> (a, Int)
  sndToInt (x, y@(sign:num)) = case sign of
    '+' -> (x, read num)
    '-' -> (x, read y)

  runProg :: Int -> Int -> Vector (String, Int) -> Set Int -> (Int, Bool)
  runProg n acc prog set
    | n >= length prog = (acc, True)
    | S.member n set = (acc, False)
    | otherwise = case opcode of
        "jmp" -> runProg (n+operand) acc prog newSet
        "acc" -> runProg (n+1) (acc+operand) prog newSet
        "nop" -> runProg (n+1) acc prog newSet
    where
      (opcode, operand) = prog ! n
      newSet = S.insert n set

  findCorrect :: Int -> Vector (String, Int) -> Int
  findCorrect n prog = case opcode of
    "acc" -> nextIndex
    "jmp" -> if term then acc else nextIndex
      where (acc, term) = runProg 0 0 (prog // [(n,("nop", operand))]) S.empty
    "nop" -> if term then acc else nextIndex
      where (acc, term) = runProg 0 0 (prog // [(n,("jmp", operand))]) S.empty
    where
      (opcode, operand) = prog ! n
      nextIndex = findCorrect (n+1) prog
