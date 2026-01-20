#!/usr/bin/env cabal
{- cabal:   
build-depends: base, split
-}

import Data.List (sort)
import Data.List.Split (splitOn)

splitAtBlank :: [String] -> ([String], [String])
splitAtBlank rows = 
    let (before, rest) = break (== "") rows
        after = drop 1 rest
    in (before, after)

inAnyRangeList :: [(Int, Int)] -> Int -> Bool
inAnyRangeList ranges x = any (\(l, r) -> x >= l && x <= r) ranges

countTrue :: [Bool] -> Int
countTrue = length . filter id

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges (x:xs) = reverse $ foldl step [x] xs
  where
    step ((lCurr, rCurr):rest) (lNext, rNext)
        | lNext <= rCurr = (lCurr, max rCurr rNext) : rest
        | otherwise = (lNext, rNext) : (lCurr, rCurr) : rest
    
    step [] (lNext, rNext) = [(lNext, rNext)]

rangeLength :: (Int, Int) -> Int
rangeLength range = snd range - fst range + 1

main :: IO ()
main = do
    contents <- readFile "input5.txt"
    let allLines = lines contents
    let (ranges, ids) = splitAtBlank allLines

    let idsInt = map read ids :: [Int]

    let rangesSplit = map (splitOn "-") ranges
    let rangesParsed = (map . map) read rangesSplit :: [[Int]]
    let rangesInt = [(x, y) | [x, y] <- rangesParsed]

    let rangesSorted = sort rangesInt
    let mergedRanges = mergeRanges rangesSorted

    let inAnyRangeListPartial = inAnyRangeList mergedRanges
    let fresh = map inAnyRangeListPartial idsInt
    print $ countTrue fresh

    let fresh2Count = sum $ map rangeLength mergedRanges
    print fresh2Count