module Program.RunDay where
import           Util.ShowString

runDay :: (ShowString out1, ShowString out2) => (String -> inp) -> (inp -> out1) -> (inp -> out2) -> String -> IO ()
runDay parser part1 part2 s = do
    let input = parser s
    putStrLn "Part 1:"
    printString $ part1 input
    putStrLn "Part 2:"
    printString $ part2 input
