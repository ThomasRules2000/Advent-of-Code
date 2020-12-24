module Main where
    import System.Environment
    import Data.Maybe
    import Text.Read

    import qualified Days.Day01 as Day01 (runDay)
    import qualified Days.Day02 as Day02 (runDay)
    import qualified Days.Day03 as Day03 (runDay)
    import qualified Days.Day04 as Day04 (runDay)
    import qualified Days.Day05 as Day05 (runDay)
    import qualified Days.Day06 as Day06 (runDay)
    import qualified Days.Day07 as Day07 (runDay)
    import qualified Days.Day08 as Day08 (runDay)
    import qualified Days.Day09 as Day09 (runDay)
    import qualified Days.Day10 as Day10 (runDay)
    import qualified Days.Day11 as Day11 (runDay)
    import qualified Days.Day12 as Day12 (runDay)
    import qualified Days.Day13 as Day13 (runDay)
    import qualified Days.Day14 as Day14 (runDay)
    import qualified Days.Day15 as Day15 (runDay)
    import qualified Days.Day16 as Day16 (runDay)
    import qualified Days.Day17 as Day17 (runDay)
    import qualified Days.Day18 as Day18 (runDay)
    import qualified Days.Day19 as Day19 (runDay)
    import qualified Days.Day20 as Day20 (runDay)
    import qualified Days.Day21 as Day21 (runDay)
    import qualified Days.Day22 as Day22 (runDay)
    import qualified Days.Day23 as Day23 (runDay)
    import qualified Days.Day24 as Day24 (runDay)
    import qualified Days.Day25 as Day25 (runDay)

    days :: [(String -> IO (), String)]
    days = [
        (Day01.runDay, "input/Day01.txt"),
        (Day02.runDay, "input/Day02.txt"),
        (Day03.runDay, "input/Day03.txt"),
        (Day04.runDay, "input/Day04.txt"),
        (Day05.runDay, "input/Day05.txt"),
        (Day06.runDay, "input/Day06.txt"),
        (Day07.runDay, "input/Day07.txt"),
        (Day08.runDay, "input/Day08.txt"),
        (Day09.runDay, "input/Day09.txt"),
        (Day10.runDay, "input/Day10.txt"),
        (Day11.runDay, "input/Day11.txt"),
        (Day12.runDay, "input/Day12.txt"),
        (Day13.runDay, "input/Day13.txt"),
        (Day14.runDay, "input/Day14.txt"),
        (Day15.runDay, "input/Day15.txt"),
        (Day16.runDay, "input/Day16.txt"),
        (Day17.runDay, "input/Day17.txt"),
        (Day18.runDay, "input/Day18.txt"),
        (Day19.runDay, "input/Day19.txt"),
        (Day20.runDay, "input/Day20.txt"),
        (Day21.runDay, "input/Day21.txt"),
        (Day22.runDay, "input/Day22.txt"),
        (Day23.runDay, "input/Day23.txt"),
        (Day24.runDay, "input/Day24.txt"),
        (Day25.runDay, "input/Day25.txt")
        ]

    main :: IO ()
    main = do
        args <- map read <$> getArgs :: IO [Int]
        mapM_ (\arg -> uncurry (performDay arg) $ fromMaybe (error "Day not found") $ lookup arg (zip [1..] days)) args

    performDay :: Int -> (String -> IO ()) -> String -> IO ()
    performDay day func inp = do
        input <- readFile inp
        putStrLn $ "=== DAY " ++ show day ++ " ==="
        func input