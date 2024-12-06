import Control.Monad.RWS.Lazy (MonadState (put))
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
  let paper_size = calculatePaperSize contents
  let ribbon_lenght = calculateRibbonLenght contents
  putStrLn $ "Part 1: " ++ show paper_size
  putStrLn $ "Part 2: " ++ show ribbon_lenght

calculatePaperSize :: String -> Int
calculatePaperSize = sum . map calculateLine . lines

calculateRibbonLenght :: String -> Int
calculateRibbonLenght = sum . map calculateRibbon . lines

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delim str =
  let (before, remainder) = break (== delim) str
   in before : case remainder of
        [] -> []
        x : xs -> split delim xs

calculateLine :: String -> Int
calculateLine line = case split 'x' line of
  [l, w, h] -> calculateArea l w h
  _ -> 0

calculateRibbon :: String -> Int
calculateRibbon line = case split 'x' line of
  [l, w, h] -> calculateLength l w h
  _ -> 0

calculateArea :: String -> String -> String -> Int
calculateArea l w h = 2 * lw + 2 * wh + 2 * hl + minimum [lw, wh, hl]
  where
    lw = read l * read w
    wh = read w * read h
    hl = read h * read l

calculateLength :: String -> String -> String -> Int
calculateLength l w h = minimum [2 * (read l + read w), 2 * (read w + read h), 2 * (read h + read l)] + read l * read w * read h