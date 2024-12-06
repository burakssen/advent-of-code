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
  let ls = lines contents
  let commands = map parseLine ls
  let grid' = foldl applyCommand grid commands
  let count = countLights grid'
  putStrLn $ "Part 1: " ++ show count

data Command = TurnOn Int Int Int Int | TurnOff Int Int Int Int | Toggle Int Int Int Int
  deriving (Show)

parseLine :: String -> Command
parseLine s = case words s of
  ["turn", "on", coord1, "through", coord2] -> parseCommand coord1 coord2 TurnOn
  ["turn", "off", coord1, "through", coord2] -> parseCommand coord1 coord2 TurnOff
  ["toggle", coord1, "through", coord2] -> parseCommand coord1 coord2 Toggle
  _ -> error $ "Invalid command: " ++ s

parseCommand :: String -> String -> (Int -> Int -> Int -> Int -> Command) -> Command
parseCommand coord1 coord2 f =
  let [x1, y1] = map read $ split ',' coord1
      [x2, y2] = map read $ split ',' coord2
   in f x1 y1 x2 y2

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delim xs =
  let (before, after) = span (/= delim) xs
   in before : split delim (drop 1 after)

type Grid = [[Bool]]

grid :: Grid
grid = replicate 1000 (replicate 1000 False)

applyCommand :: Grid -> Command -> Grid
applyCommand g (TurnOn x1 y1 x2 y2) = updateGrid g x1 y1 x2 y2 (const True)
applyCommand g (TurnOff x1 y1 x2 y2) = updateGrid g x1 y1 x2 y2 (const False)
applyCommand g (Toggle x1 y1 x2 y2) = updateGrid g x1 y1 x2 y2 not

updateGrid :: Grid -> Int -> Int -> Int -> Int -> (Bool -> Bool) -> Grid
updateGrid g x1 y1 x2 y2 f =
  let rows = take (y2 - y1 + 1) $ drop y1 g
      rows' = map (updateRow x1 x2 f) rows
   in take y1 g ++ rows' ++ drop (y2 + 1) g

updateRow :: Int -> Int -> (Bool -> Bool) -> [Bool] -> [Bool]
updateRow x1 x2 f row =
  let (left, right) = splitAt x1 row
      (middle, right') = splitAt (x2 - x1 + 1) right
   in left ++ map f middle ++ right'

countLights :: Grid -> Int
countLights g = length $ concatMap (filter id) g