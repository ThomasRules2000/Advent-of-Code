module Day22 where
  import Data.List.Split
  import Data.Vector (Vector)
  import qualified Data.Vector as Vec
  import Data.List (sortBy)
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Data.Sequence (Seq(..), (<|), (|>))
  import qualified Data.Sequence as Seq
  import Data.Foldable (toList)

  main :: IO ()
  main = do
    [p1, p2] <- map (Seq.fromList . map read . tail . lines) . splitOn "\n\n" <$> readFile "input.txt" :: IO [Seq Int]
    print $ combat p1 p2
    print $ calcScore $ fst $ recCombat p1 p2 Set.empty

  combat :: Seq Int -> Seq Int -> Int
  combat Empty p2 = calcScore p2
  combat p1 Empty = calcScore p1
  combat (x:<|p1) (y:<|p2)
    | x > y = combat (p1|>x|>y) p2
    | otherwise = combat p1 $ p2|>y|>x

  calcScore :: Seq Int -> Int
  calcScore = Vec.sum . Vec.imap (*) . Vec.fromList . (0:) . reverse . toList

  recCombat :: Seq Int -> Seq Int -> Set (Seq Int, Seq Int) -> (Seq Int, Bool)
  recCombat Empty p2 _ = (p2, False)
  recCombat p1 Empty _ = (p1, True)
  recCombat p1@(x:<|xs) p2@(y:<|ys) prevRounds
    | (p1,p2) `Set.member` prevRounds = (p1, True)
    | x <= Seq.length xs && y <= Seq.length ys = if snd $ recCombat (Seq.take x xs) (Seq.take y ys) Set.empty then p1Win else p2Win
    | x > y = p1Win
    | otherwise = p2Win
    where
      newSet = Set.insert (p1,p2) prevRounds
      p1Win = recCombat (xs|>x|>y) ys newSet
      p2Win = recCombat xs (ys|>y|>x) newSet
