module Main where
    import Days.Day01 as Day01 (runDay)
    import Days.Day02 as Day02 (runDay)
    import Days.Day03 as Day03 (runDay)
    import Days.Day04 as Day04 (runDay)
    import Days.Day05 as Day05 (runDay)
    import Days.Day06 as Day06 (runDay)
    import Days.Day07 as Day07 (runDay)
    import Days.Day08 as Day08 (runDay)
    import Days.Day09 as Day09 (runDay)
    import Days.Day10 as Day10 (runDay)
    import Days.Day11 as Day11 (runDay)
    import Days.Day12 as Day12 (runDay)
    import Days.Day13 as Day13 (runDay)
    import Days.Day14 as Day14 (runDay)
    import Days.Day15 as Day15 (runDay)
    import Days.Day16 as Day16 (runDay)
    import Days.Day17 as Day17 (runDay)
    import Days.Day18 as Day18 (runDay)
    import Days.Day19 as Day19 (runDay)
    import Days.Day20 as Day20 (runDay)
    import Days.Day21 as Day21 (runDay)
    import Days.Day22 as Day22 (runDay)

    main :: IO ()
    main = do
        file <- readFile "../../input/Day22.txt"
        Day22.runDay file