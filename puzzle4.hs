import qualified Data.Set as Set

type Coord = (Int, Int)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

intToBool :: Int -> Bool
intToBool 1 = True
intToBool 0 = False
intToBool _ = False

getNeighbors :: Coord -> [Coord]
getNeighbors (x, y) = [(x + z1, y + z2) 
                      | z1 <- [-1, 0, 1]
                      , z2 <- [-1, 0, 1]
                      , z1 /= 0 || z2 /= 0
                      ]

isPaperAtSet :: Set.Set Coord -> Coord -> Int
isPaperAtSet paperSet coord = if Set.member coord paperSet then 1 else 0

countAdjacentPaperSet :: Set.Set Coord -> Coord -> Int
countAdjacentPaperSet paperSet (x, y) = 
    sum $ map (isPaperAtSet paperSet) (getNeighbors (x, y))

step :: Set.Set Coord -> Set.Set Coord
step currentSet = Set.filter (\coord -> countAdjacentPaperSet currentSet coord >= 4) currentSet

stabilize :: Set.Set Coord -> Set.Set Coord
stabilize coords =
    let next = step coords
    in if next == coords then coords else stabilize next

main :: IO ()
main = do
    contents <- readFile "input4.txt"
    let grid = lines contents
    let initialCoords = Set.fromList [ (r, c) 
                                     | (r, row) <- zip [0..] grid
                                     , (c, col) <- zip [0..] row
                                     , col == '@'
                                     ]

    let finalState = stabilize initialCoords
    
    print $ Set.size initialCoords - Set.size finalState