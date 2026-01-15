#!/usr/bin/env cabal
{- cabal:   
build-depends: base, split, unordered-containers
-}

import Data.List.Split (splitOn)
import Data.Char (digitToInt)
import Data.List (concat)
import Data.HashSet (HashSet, fromList, toList)


main :: IO ()
main = do
    contents <- readFile "input2.txt"
    let ranges = splitOn "," contents
    let rangesSplit = map (splitOn "-") ranges
    let rangesParsed = (map . map) read rangesSplit :: [[Integer]]
    let pairs = [(x, y) | [x, y] <- rangesParsed]

    let mMax = maximum $ map length $ concat rangesSplit

    let seeds = map show [1..10^(mMax `div` 2 + 1)]

    let invalidIdsStr = [ concat $ replicate m s | m <- [2..mMax], s <- seeds ]
    let invalidIdsStrFiltered = filter (\x -> length x <= mMax) invalidIdsStr
    let invalidIds = map read invalidIdsStrFiltered :: [Integer]
    let invalidIdsSet = fromList invalidIds

    let masterList = toList invalidIdsSet

    let results = map (\(l, r) -> sum [v | v <- masterList, v >= l, v <= r]) pairs

    print (sum results)
