import Data.List
import Data.Maybe
import Data.Map
import Data.Set
import Data.Char
import Debug.Trace

{-

-- Find the number of trees that are visible.
-- Each cell represents the height of a tree.
-- All trees on the outside are visible.

-}

{-

-- Elves also want to be able to see a lot of trees.
-- Look in four directions from tree, stop at edge or tree
    with greater or equal height.
-- Scenic score found by multiplying its viewing distance
    in each of the four directions.
-- Find the highest possible scenic score for any tree on
    the map.

-}


data Direction = Top | Down | Left_ | Right_

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- contents <- readFile "sample-input.txt"
  let gridSize = length (lines contents)
  let grid = parseInput (lines contents) 0
  let borderTrees = addBorderTrees 0 gridSize
  let visibleInteriorTrees = findVisibleInteriorTrees grid gridSize (1, 1)
  let visibleTrees = Data.Set.union borderTrees visibleInteriorTrees 
  -- print (Data.Set.size visibleTrees)
  print (findHighestScenicScore grid gridSize 0 (0, 0))

findHighestScenicScore :: Map (Int, Int) Int -> Int -> Int -> (Int, Int) -> Int
findHighestScenicScore grid gridSize bestScenicScore (currRow, currCol)
  | currRow >= gridSize || currCol >= gridSize = bestScenicScore -- base case
  | otherwise = 
      let updatedBestScenicScore = Data.List.maximum [findHighestScenicScoreRow grid gridSize (currRow, currCol), findHighestScenicScoreCol grid gridSize (currRow, currCol), bestScenicScore]
      in max updatedBestScenicScore (findHighestScenicScore grid gridSize updatedBestScenicScore (currRow + 1, currCol + 1))

findHighestScenicScoreRow :: Map (Int, Int) Int -> Int -> (Int, Int) -> Int
findHighestScenicScoreRow grid gridSize (currRow, currCol) 
  | currCol < gridSize = 
    max 
    (findScenicScore grid gridSize (fromJust currTreeHeight) (currRow, currCol))
    (findHighestScenicScoreRow grid gridSize (currRow, currCol + 1))
  | otherwise = 0
  where
    currTreeHeight = Data.Map.lookup (currRow, currCol) grid

findHighestScenicScoreCol :: Map (Int, Int) Int -> Int -> (Int, Int) -> Int
findHighestScenicScoreCol grid gridSize (currRow, currCol) 
  | currRow < gridSize = 
    max 
    (findScenicScore grid gridSize (fromJust currTreeHeight) (currRow, currCol))
    (findHighestScenicScoreCol grid gridSize (currRow + 1, currCol))
  | otherwise = 0
  where
    currTreeHeight = Data.Map.lookup (currRow, currCol) grid

findScenicScore :: Map (Int, Int) Int -> Int -> Int -> (Int, Int) -> Int
findScenicScore grid gridSize treeHeight (row, col) = 
  numberVisibleDirection grid gridSize treeHeight (row - 1, col) Top
  * numberVisibleDirection grid gridSize treeHeight (row + 1, col) Down
  * numberVisibleDirection grid gridSize treeHeight (row, col - 1) Left_
  * numberVisibleDirection grid gridSize treeHeight (row, col + 1) Right_

numberVisibleDirection :: Map (Int, Int) Int -> Int -> Int -> (Int, Int) -> Direction -> Int
numberVisibleDirection grid gridSize treeHeight (currRow, currCol) direction = case direction of
  Top -> 
    if currRow >= 0 && fromJust currTreeHeight < treeHeight
    then 1 + numberVisibleDirection grid gridSize treeHeight (currRow - 1, currCol) Top
    else endedAtTallerTree (currRow <= 0)
  Down ->
    if currRow < gridSize && fromJust currTreeHeight < treeHeight
    then 1 + numberVisibleDirection grid gridSize treeHeight (currRow + 1, currCol) Down
    else endedAtTallerTree (currRow >= gridSize)
  Left_ -> 
    if currCol >= 0 && fromJust currTreeHeight < treeHeight
    then 1 + numberVisibleDirection grid gridSize treeHeight (currRow, currCol - 1) Left_
    else endedAtTallerTree (currCol <= 0)
  Right_ ->
    if currCol < gridSize && fromJust currTreeHeight < treeHeight
    then 1 + numberVisibleDirection grid gridSize treeHeight (currRow, currCol + 1) Right_
    else endedAtTallerTree (currCol >= gridSize)
  where
    endedAtTallerTree :: Bool -> Int
    endedAtTallerTree outOfBounds = if outOfBounds then 0 else 1
    currTreeHeight = Data.Map.lookup (currRow, currCol) grid

parseInput :: [String] -> Int -> Map (Int, Int) Int
parseInput [] _ = Data.Map.empty
parseInput (row:otherRows) currRow = 
  Data.Map.union (parseInputRow row currRow 0) (parseInput otherRows (currRow + 1))

parseInputRow :: String -> Int -> Int-> Map (Int, Int) Int
parseInputRow [] _ _ = Data.Map.empty
parseInputRow (currTreeHeight:otherTreeHeights) currRow currCol =
  Data.Map.insert (currRow, currCol) (digitToInt currTreeHeight) 
  (parseInputRow otherTreeHeights currRow (currCol + 1))

addBorderTrees :: Int -> Int -> Set (Int, Int)
addBorderTrees currCell gridSize
  | currCell < gridSize = 
    Data.Set.union 
    (Data.Set.fromList [(currCell, 0), (currCell, gridSize - 1), (0, currCell), (gridSize - 1, currCell)])
    (addBorderTrees (currCell + 1) gridSize)
  | otherwise = Data.Set.empty

findVisibleInteriorTrees :: Map (Int, Int) Int -> Int -> (Int, Int) -> Set (Int, Int) 
findVisibleInteriorTrees grid gridSize (currRow, currCol) 
  | currRow < gridSize && currCol < gridSize = 
    Data.Set.unions [visibleFromLeft, visibleFromRight, visibleFromTop,
    visibleFromDown, findVisibleInteriorTrees grid gridSize (currRow + 1, currCol + 1)]
  | otherwise = Data.Set.empty
  where
    visibleFromLeft = 
      traverseInteriorRowLeft grid gridSize (fromJust (Data.Map.lookup (currRow, 0) grid)) (currRow, 1)
    visibleFromRight = 
      traverseInteriorRowRight grid (fromJust (Data.Map.lookup (currRow, gridSize - 1) grid)) (currRow, gridSize - 2)
    visibleFromTop = 
      traverseInteriorColDown grid gridSize (fromJust (Data.Map.lookup (0, currCol) grid)) (1, currCol)
    visibleFromDown = 
      traverseInteriorColUp grid (fromJust (Data.Map.lookup (gridSize - 1, currCol) grid))  (gridSize - 2, currCol)

traverseInteriorRowLeft :: Map (Int, Int) Int -> Int -> Int -> (Int, Int) -> Set (Int, Int)
traverseInteriorRowLeft grid maxCols maxHeight (currRow, currCol)
  | currCol < maxCols && maxHeight < fromJust currTreeHeight = 
    Data.Set.insert (currRow, currCol) 
    (traverseInteriorRowLeft grid maxCols (fromJust currTreeHeight) (currRow, currCol + 1))
  | currCol < maxCols = traverseInteriorRowLeft grid maxCols maxHeight (currRow, currCol + 1)
  | otherwise = Data.Set.empty 
  where 
    currTreeHeight = Data.Map.lookup (currRow, currCol) grid

traverseInteriorRowRight :: Map (Int, Int) Int -> Int -> (Int, Int) -> Set (Int, Int)
traverseInteriorRowRight grid maxHeight (currRow, currCol)
  | currCol >= 0 && maxHeight < fromJust currTreeHeight = 
    Data.Set.insert (currRow, currCol) 
    (traverseInteriorRowRight grid (fromJust currTreeHeight) (currRow, currCol - 1))
  | currCol >= 0 = traverseInteriorRowRight grid maxHeight (currRow, currCol - 1)
  | otherwise = Data.Set.empty 
  where 
    currTreeHeight = Data.Map.lookup (currRow, currCol) grid

traverseInteriorColDown :: Map (Int, Int) Int -> Int -> Int -> (Int, Int) -> Set (Int, Int)
traverseInteriorColDown grid maxRows  maxHeight (currRow, currCol)
  | currRow < maxRows && maxHeight < fromJust currTreeHeight = 
    Data.Set.insert (currRow, currCol) 
    (traverseInteriorColDown grid maxRows (fromJust currTreeHeight) (currRow + 1, currCol))
  | currRow < maxRows = traverseInteriorColDown grid maxRows maxHeight (currRow + 1, currCol)
  | otherwise = Data.Set.empty 
  where 
    currTreeHeight = Data.Map.lookup (currRow, currCol) grid

traverseInteriorColUp :: Map (Int, Int) Int -> Int -> (Int, Int) -> Set (Int, Int)
traverseInteriorColUp grid maxHeight (currRow, currCol)
  | currRow >= 0 && maxHeight < fromJust currTreeHeight = 
    Data.Set.insert (currRow, currCol) 
    (traverseInteriorColUp grid (fromJust currTreeHeight) (currRow - 1, currCol))
  | currRow >= 0 = traverseInteriorColUp grid maxHeight (currRow - 1, currCol)
  | otherwise = Data.Set.empty 
  where 
    currTreeHeight = Data.Map.lookup (currRow, currCol) grid


-- Number of visible tree is 1705!
-- Highest scenic score is 371200!