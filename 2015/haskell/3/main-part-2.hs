import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Environment (getArgs)

data Pos = Pos {x :: Int, y :: Int}
  deriving (Eq, Ord, Show)

type GridValue = Int

type Grid = Map Pos GridValue

emptyGrid :: Grid
emptyGrid = Map.empty

setGrid :: Grid -> Pos -> GridValue -> Grid
setGrid grid pos value = Map.insert pos value grid

getGrid :: Grid -> Pos -> Maybe GridValue
getGrid grid pos = Map.lookup pos grid

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> processFile filename
    _ -> putStrLn "Usage: main <input_file>"

processFile :: FilePath -> IO ()
processFile filename = do
  contents <- readFile filename
  let santa = Pos 0 0
  let robo_santa = Pos 0 0
  let santa_turn = True
  let count = processContents (santa, robo_santa) santa_turn emptyGrid contents 0
  putStrLn $ "Part 2: " ++ show count

processContents :: (Pos, Pos) -> Bool -> Grid -> String -> Int -> Int
processContents (santa, robo_santa) santa_turn grid [] count = count
processContents (santa, robo_santa) santa_turn grid (c : cs) count =
  let currentPos = if santa_turn then santa else robo_santa
      nextPos = getNextPos currentPos c
      currentValue = Map.findWithDefault 0 nextPos grid
      newCount = if currentValue == 0 then count + 1 else count
      newGrid = setGrid grid nextPos (currentValue + 1)
   in processContents (if santa_turn then (nextPos, robo_santa) else (santa, nextPos)) (not santa_turn) newGrid cs newCount

getNextPos :: Pos -> Char -> Pos
getNextPos currentPos c = case c of
  '^' -> Pos (x currentPos) (y currentPos + 1)
  'v' -> Pos (x currentPos) (y currentPos - 1)
  '>' -> Pos (x currentPos + 1) (y currentPos)
  '<' -> Pos (x currentPos - 1) (y currentPos)
  _ -> currentPos