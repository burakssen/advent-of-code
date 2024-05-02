import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: main <input_file>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let (floorNumber, basement) = calculateFloor (filter (`elem` "()") contents)
  putStrLn $ "Floor: " ++ show floorNumber
  putStrLn $ "Basement: " ++ show basement

calculateFloor :: String -> (Int, Int)
calculateFloor input = go input (0, 0)
  where
    go [] (floor, basement) = (floor, basement)
    go (x : xs) (floor, basement)
      | floor == -1 && basement == 0 = go xs (floor - 1, length input - length xs - 1)
      | x == '(' = go xs (floor + 1, basement)
      | x == ')' = go xs (floor - 1, basement)
      | otherwise = go xs (floor, basement)