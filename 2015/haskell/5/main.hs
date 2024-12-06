import System.Environment (getArgs)
import Data.List (tails, elemIndex)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: main <input_file>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let good_strings1 = filter check_good_string_part1 $ lines contents
  let good_strings2 = filter check_good_string_part2 $ lines contents
  putStrLn $ "Part 1: " ++ show (length good_strings1)
  putStrLn $ "Part 2: " ++ show (length good_strings2)


check_good_string_part1 :: String -> Bool
check_good_string_part1 s =
    let vowels = length $ filter (`elem` "aeiou") s
        double = any (uncurry (==)) $ zip s (tail s)
        bad = any ((`elem` ["ab", "cd", "pq", "xy"]) . (\(a, b) -> [a, b])) (zip s (tail s))
    in vowels >= 3 && double && not bad


check_good_string_part2 :: String -> Bool
check_good_string_part2 s = pair && repeat
  where
    pair = any (\(i, j) -> i + 1 < length s && j + 1 < length s && s !! i == s !! j && s !! (i + 1) == s !! (j + 1)) pairs
    repeat = any (\i -> i + 2 < length s && s !! i == s !! (i + 2)) [0..length s - 2]
    pairs = [(i, j) | i <- [0..length s - 2], j <- [i + 2..length s - 1]]
