import Data.ByteString.Char8 (pack)
import Distribution.Utils.MD5 (md5)
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
  -- calculate the first 5 and 6 leading zeros of the md5 hash
  let leading_5 = calculateMD5 contents 5
  let leading_6 = calculateMD5 contents 6
  putStrLn $ "Part 1: " ++ show leading_5
  putStrLn $ "Part 2: " ++ show leading_6

-- [x|x<-[0..] infinite loop
-- let hash = md5 $ pack $ contents ++ show x calculate the md5 hash of the input string + x
-- all (== '0') $ take n $ show hash check if the first n characters of the hash are all 0
calculateMD5 :: String -> Int -> Int
calculateMD5 contents n = head [x | x <- [0 ..], let hash = md5 $ pack $ contents ++ show x, all (== '0') $ take n $ show hash]
