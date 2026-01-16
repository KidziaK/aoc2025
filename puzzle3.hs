findJoltage :: String -> Int -> String
findJoltage _ 0 = ""
findJoltage "" _ = ""
findJoltage n k = 
    let
        windowSize = length n - k + 1
        window = take windowSize n

        maxChar = maximum window

        (_, firstMatchAndResult) = break (== maxChar) n
        rest = drop 1 firstMatchAndResult
        
    in maxChar : findJoltage rest (k - 1)

main :: IO ()
main = do
    contents <- readFile "input3.txt"
    let banks = lines contents
    let joltagesStr = map (`findJoltage` 12) banks
    let joltages = map read joltagesStr :: [Int]
    let result = sum joltages
    print result
