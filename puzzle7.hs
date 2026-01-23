import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Language.Haskell.TH (match)
import Data.Array (Array, array, (!), indices, bounds)

type Grid = [String]

next :: Char -> (Int, Int) -> [(Int, Int)]
next '^' (x, y) = [(x - 1, y), (x + 1, y)]
next _   (x, y) = [(x, y + 1)]

rayTraceMemo :: Grid -> (Int, Int) -> Int
rayTraceMemo grid (startX, startY) = memo ! (startX, startY)
  where
    height = length grid
    width  = length (head grid)
    
    memo :: Array (Int, Int) Int
    memo = array ((0, 0), (width - 1, height - 1)) 
                 [((x, y), compute x y) | x <- [0..width-1], y <- [0..height-1]]

    compute x y = sum [lookupVal nextStep | nextStep <- nextSteps]
      where
        currentChar = (grid !! y) !! x
        nextSteps   = next currentChar (x, y)
        
        lookupVal (nx, ny)
            | ny >= height          = 1  
            | nx < 0 || nx >= width = 0  
            | otherwise             = memo ! (nx, ny)

main :: IO ()
main = do
    contents <- readFile "input7.txt"
    let grid = lines contents

    let startIndexCol = fromJust $ elemIndex 'S' (head grid)
    let startIndexRow = 0

    let startIndex = (startIndexCol, startIndexRow)

    print $ rayTraceMemo grid startIndex