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
  let floorNumber = calculateFloor (filter (`elem` "()") contents)
  print floorNumber

