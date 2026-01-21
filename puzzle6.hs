#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}

import Data.List (transpose, find)
import Data.List.Split (splitWhen)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

parseVerticalNumber :: String -> Maybe Int
parseVerticalNumber col = 
    let digits = filter isDigit col
    in if null digits then Nothing else Just (read digits)

getOperator :: [String] -> ([Int] -> Int)
getOperator cols =
    let bottomRow = map last cols
    in case find (`elem` "+*") bottomRow of
        Just '*' -> product
        Just '+' -> sum
        _        -> sum

main :: IO ()
main = do
    contents <- readFile "input6.txt"
    let rawLines = lines contents
    let width = maximum (map length rawLines)
    let grid = map (\l -> l ++ replicate (width - length l) ' ') rawLines
    
    let allCols = transpose grid
    
    let isSpacer col = all (== ' ') (init col)
    let blocks = filter (not . null) $ splitWhen isSpacer allCols
    
    let solveBlock cols =
            let nums = map (fromMaybe 0 . parseVerticalNumber . init) (reverse cols)
                cleanNums = filter (> 0) nums 
                op = getOperator cols
            in op cleanNums

    let results = map solveBlock blocks
    print $ sum results