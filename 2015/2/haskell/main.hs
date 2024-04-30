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
  let total = calculateTotal contents
  print total

calculateTotal :: String -> Int
calculateTotal = sum . map calculateLine . lines

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

calculateArea :: String -> String -> String -> Int
calculateArea l w h = 2 * lw + 2 * wh + 2 * hl + minimum [lw, wh, hl]
  where
    lw = read l * read w
    wh = read w * read h
    hl = read h * read l