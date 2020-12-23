module Program.RunDay where
    runDay :: (Show out1, Show out2) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> String -> IO ()
    runDay parser part1 part2 s = do
        let input = parser s
        putStrLn "Part 1:"
        print $ part1 input
        putStrLn "Part 2:"
        print $ part2 input