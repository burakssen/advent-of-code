import System.Environment (getArgs)
import System.IO (hClose, openFile, hGetContents, IOMode(ReadMode))
import Data.List (foldl')

-- countChars calculates the number of characters in the code and the
-- number of characters in the string representation, excluding escape sequences.
countChars :: String -> (Int, Int) -> (Int, Int)
countChars s (codeChars, stringChars) = 
    let newCodeChars = codeChars + length s
        newStringChars = stringChars + countStringChars (init (tail s)) 0
    in (newCodeChars, newStringChars)

countStringChars :: String -> Int -> Int
countStringChars [] count = count
countStringChars ('\\':'\\':xs) count = countStringChars xs (count + 1)
countStringChars ('\\':'"':xs) count = countStringChars xs (count + 1)
countStringChars ('\\':'x':_:_:xs) count = countStringChars xs (count + 1)
countStringChars (_:xs) count = countStringChars xs (count + 1)

-- encodeString returns an encoded version of the input string and its length.
encodeString :: String -> (String, Int)
encodeString s = 
    let encoded = '"' : concatMap encodeChar s ++ "\""
    in (encoded, length encoded)

encodeChar :: Char -> String
encodeChar '"'  = "\\\""
encodeChar '\\' = "\\\\"
encodeChar c    = [c]

main :: IO ()
main = do
    args <- getArgs
    if null args then
        putStrLn "missing input file"
    else do
        let inputFile = head args
        handle <- openFile inputFile ReadMode
        contents <- hGetContents handle

        let linesOfFiles = lines contents

        let (totalCodeChars, totalStringChars) = foldl' (\acc line -> countChars line acc) (0, 0) linesOfFiles
        let totalOrigChars = sum (map length linesOfFiles)
        let totalEncChars = sum (map (snd . encodeString) linesOfFiles)

        putStrLn $ "Part 1: " ++ show (totalCodeChars - totalStringChars)
        putStrLn $ "Part 2: " ++ show (totalEncChars - totalOrigChars)

        hClose handle
