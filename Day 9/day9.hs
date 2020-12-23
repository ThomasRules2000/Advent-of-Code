{-# LANGUAGE ApplicativeDo   #-}
module Day9 where
  import Data.Sequence (Seq(..), (|>), (<|))
  import qualified Data.Sequence as Seq

  main :: IO ()
  main = do
    nums <- Seq.fromList . map read . lines <$> readFile "input.txt" :: IO (Seq Int)
    let invalid = uncurry findInvalid $ Seq.splitAt 25 nums
    print invalid
    print $ findWeakness invalid nums

  findInvalid :: Seq Int -> Seq Int -> Int
  findInvalid ss@(_:<|rest) (x:<|xs)
    | isValid x ss = findInvalid (rest|>x) xs
    | otherwise = x
    where
      isValid :: Int -> Seq Int -> Bool
      isValid n ss = elem n $ do { x <- ss; y <- ss; return (x+y) }

  findWeakness :: Int -> Seq Int -> Int
  findWeakness n xs@(_:<|ts) = case Seq.elemIndexL n $ Seq.scanl (+) 0 xs of
    Nothing -> findWeakness n ts
    Just i -> maximum range + minimum range
      where range = Seq.take i xs
