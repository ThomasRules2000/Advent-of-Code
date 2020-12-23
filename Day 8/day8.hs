{-# LANGUAGE ApplicativeDo   #-}
module Day8 where
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec
  import Data.IntSet (IntSet)
  import qualified Data.IntSet as IntSet

  data Instruction = Jmp Int
                   | Acc Int
                   | Nop Int
                   deriving (Eq, Show)

  main :: IO ()
  main = do
    prog <- Vec.fromList . map (getInstruction . listToTuple . words) . lines <$> readFile "input.txt"
    print $ fst $ runProg 0 0 prog IntSet.empty
    print $ findCorrect 0 prog

  listToTuple :: [a] -> (a,a)
  listToTuple [x,y] = (x,y)

  getInstruction :: (String, String) -> Instruction
  getInstruction (opcode, whole@(sign:num)) = case opcode of
    "jmp" -> Jmp n
    "acc" -> Acc n
    "nop" -> Nop n
    where n = case sign of
              '+' -> read num
              '-' -> read whole

  runProg :: Int -> Int -> Vector Instruction -> IntSet -> (Int, Bool)
  runProg n acc prog set
    | n >= length prog = (acc, True)
    | IntSet.member n set = (acc, False)
    | otherwise = case prog Vec.! n of
        Jmp operand -> runProg (n+operand) acc prog newSet
        Acc operand -> runProg (n+1) (acc+operand) prog newSet
        Nop operand -> runProg (n+1) acc prog newSet
    where newSet = IntSet.insert n set

  findCorrect :: Int -> Vector Instruction -> Int
  findCorrect n prog = case prog Vec.! n of
    Acc _ -> nextIndex
    Jmp operand -> if term then acc else nextIndex
      where (acc, term) = runProg 0 0 (prog Vec.// [(n, Nop operand)]) IntSet.empty
    Nop operand -> if term then acc else nextIndex
      where (acc, term) = runProg 0 0 (prog Vec.// [(n, Jmp operand)]) IntSet.empty
    where nextIndex = findCorrect (n+1) prog
