import System.IO (readFile)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (foldl')

parseToInt :: String -> Int
parseToInt s = fromMaybe 0 (readMaybe s)

codeToNumber :: String -> Int
codeToNumber (dir : valStr)
    | dir == 'L' = -(parseToInt valStr)
    | dir == 'R' = parseToInt valStr
    | otherwise = 0
codeToNumber "" = 0

main :: IO ()
main = do
    contents <- readFile "input1.txt"
    let allLines = lines contents
    
    let startingPos = 50 

    let (totalZeros, finalPos) = foldl' (\(count, currentPos) line ->
            let movement = codeToNumber line
                nextPos  = currentPos + movement

                hits | movement > 0 = (nextPos `div` 100) - (currentPos `div` 100)
                     | movement < 0 = ((currentPos - 1) `div` 100) - ((nextPos - 1) `div` 100)
                     | otherwise    = 0
            
            in (count + hits, nextPos)
            
          ) (0, startingPos) allLines

    putStrLn "The Part 2 password is:"
    print totalZeros