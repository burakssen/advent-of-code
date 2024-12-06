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
  let currentPos = Pos 0 0
  let count = processContents currentPos emptyGrid contents 0
  putStrLn $ "Part 1: " ++ show count

processContents :: Pos -> Grid -> String -> Int -> Int
processContents currentPos grid [] count = count
processContents currentPos grid (c : cs) count =
  case c of
    '^' -> processDirection currentPos (\pos -> Pos (x pos) (y pos + 1)) grid cs count
    'v' -> processDirection currentPos (\pos -> Pos (x pos) (y pos - 1)) grid cs count
    '>' -> processDirection currentPos (\pos -> Pos (x pos + 1) (y pos)) grid cs count
    '<' -> processDirection currentPos (\pos -> Pos (x pos - 1) (y pos)) grid cs count
    _ -> processContents currentPos grid cs count

processDirection :: Pos -> (Pos -> Pos) -> Grid -> String -> Int -> Int
processDirection currentPos getNewPos grid cs count =
  let newPos = getNewPos currentPos
      currentValue = Map.findWithDefault 0 newPos grid
      newCount = if currentValue == 0 then count + 1 else count
   in processContents newPos (setGrid grid newPos (currentValue + 1)) cs newCount