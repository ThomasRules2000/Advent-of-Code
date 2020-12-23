{-# LANGUAGE ApplicativeDo   #-}
module Day10 where
  import Data.List (sort)
  import Data.Sequence (Seq(..), (|>), (<|), ViewR(..))
  import qualified Data.Sequence as Seq
  import Data.Map.Strict (Map)
  import qualified Data.Map.Strict as Map
  import Data.List.Split

  main :: IO ()
  main = do
    adapters <- Seq.fromList . sort . map read . lines <$> readFile "input.txt" :: IO (Seq Int)
    let (_:>devJolts) = (+3) <$> Seq.viewr adapters
    let differences = getDifferences devJolts adapters
    print $ Seq.length (Seq.filter (==1) differences) * Seq.length (Seq.filter (==3) differences)
    print $ dynamicProgramming (Map.singleton devJolts 1) $ 0 <| adapters

  getDifferences :: Int -> Seq Int -> Seq Int
  getDifferences devJolts ss = Seq.zipWith (-) (ss |> devJolts) (0 <| ss)

  dynamicProgramming :: Map Int Int -> Seq Int -> Int
  dynamicProgramming m Empty = m Map.! 0
  dynamicProgramming m (xs:|>x) = dynamicProgramming (Map.insert x (sum $ map (flip (Map.findWithDefault 0) m . (x+)) [1..3]) m) xs
