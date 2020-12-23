{-# LANGUAGE ApplicativeDo   #-}
module Days.Day01 where
  import qualified Program.RunDay as R (runDay)

  runDay :: String -> IO ()
  runDay = R.runDay parser part1 part2

  type Input = [Int]

  type Output1 = Int
  type Output2 = Int

  parser :: String -> Input
  parser = map read . lines

  part1 :: Input -> Output1
  part1 nums = head [x*y | x <- nums, y <- nums, x+y==2020]

  part2 :: Input -> Output2
  part2 nums = head [x*y*z | x <- nums, y <- nums, z <- nums, x+y+z==2020]
