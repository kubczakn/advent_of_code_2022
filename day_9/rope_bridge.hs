import Data.List
import Data.Maybe
import Data.Map
import Data.Set
import Debug.Trace

{-

* Head and tail of a rope must also be touching,
    (diagonally adjacent and overlapping count)
* If head aren't touching and are in the same row, the 
    tail must also move in the direction of the tail to
    keep touching
* Otherwise, if not touching and not in the same row, the
    the tail must move diagonally to keep touching
* Need to work out where the tail goes as the head follows
    a series of motions (right, left, up, down)
* After simulating the rope, you can count all of the positions
    the tail visited at least once
* Find how many positions the tail visits at least once after
    the simulation

-}

{-

* Rope has nine knots now, meaning that there is now a gap
  of 7 knots between the head and the tail.
* Intuition: Each knot pair retains the head/tail relationship.

-}

{-

* Strategy:
    - Assume start at (0,0), head and tail initially overlapping
    - If same row/col, move tail amount needed to reach head
    - If not same row/col, move tail diagonally to be in same row/col
-}

-- TODO: Compare path tail took in simulation to correct path 

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- contents <- readFile "sample-input.txt"
  -- contents <- readFile "sample-input-2.txt"
  let initialKnotPositions = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]
  -- let visitedTailPositions = simulateMoves (lines contents) (0, 0) (0, 0) (Data.Set.fromList [(0,0)])
  let visitedTailPositions = simulateMovesMoreKnots (lines contents) initialKnotPositions (Data.Set.fromList [(0,0)])
  -- print visitedTailPositions
  print (Data.Set.size visitedTailPositions)

simulateMovesMoreKnots :: [String] -> [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
simulateMovesMoreKnots [] knotPositions positionsTailVisited = -- trace (show knotPositions) 
  positionsTailVisited 
simulateMovesMoreKnots (currentMove:otherMoves) knotPositions positionsTailVisited =
  simulateMovesMoreKnots otherMoves updatedKnots updatedTailPositions 
  where
    newHeadPosition = makeMove currentMove (head knotPositions) 
    headKnotUpdated = newHeadPosition : tail knotPositions 
    updatedMiddleKnotPositions = updateMiddleKnots headKnotUpdated
    tailPair = Data.List.drop 8 updatedMiddleKnotPositions
    (updatedTailPositions, updatedTailPosition) = updateTailPosition (head tailPair) (last tailPair) positionsTailVisited 
    updatedKnots = init updatedMiddleKnotPositions ++ [updatedTailPosition]

updateMiddleKnots :: [(Int, Int)] -> [(Int, Int)]
updateMiddleKnots (knotOne:knotTwo:otherKnots)
  | Data.List.null otherKnots = [knotOne, knotTwo]
  | otherwise = 
    -- let newKnotTwoPosition = updateMiddleKnotPosition knotOne knotTwo in
    let (_, newKnotTwoPosition) = updateTailPosition knotOne knotTwo Data.Set.empty in
    knotOne : updateMiddleKnots (newKnotTwoPosition : otherKnots)

simulateMoves :: [String] -> (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
simulateMoves [] _ _ positionsTailVisited = positionsTailVisited 
simulateMoves (currentMove:otherMoves) headPosition tailPosition positionsTailVisited =
  simulateMoves otherMoves newHeadPosition newTailPosition updatedTailPositions 
  where
    newHeadPosition = makeMove currentMove headPosition
    (updatedTailPositions, newTailPosition) = 
      updateTailPosition newHeadPosition tailPosition positionsTailVisited

isTailAdjacent :: (Int, Int) -> (Int, Int) -> Bool
isTailAdjacent (headX, headY) (tailX, tailY) = xDiff <= 1 && yDiff <= 1 
  where
    xDiff = abs (headX - tailX)
    yDiff = abs (headY - tailY)

updateMiddleKnotPosition :: (Int, Int) -> (Int, Int) -> (Int, Int)
updateMiddleKnotPosition (headX, headY) (tailX, tailY) 
  | isTailAdjacent (headX, headY) (tailX, tailY) = (tailX, tailY)
  | headX == tailX || headY == tailY = 
    let newTailPosition = moveTailSameRowCol (headX, headY) (tailX, tailY) in
    updateMiddleKnotPosition (headX, headY) newTailPosition
  | otherwise = 
    let newTailPosition = moveTailDiagonally (headX, headY) (tailX, tailY) in
    updateMiddleKnotPosition (headX, headY) newTailPosition

updateTailPosition :: (Int, Int) -> (Int, Int) -> Set (Int, Int) -> (Set (Int, Int), (Int, Int))
updateTailPosition (headX, headY) (tailX, tailY) positionsTailVisited
  | isTailAdjacent (headX, headY) (tailX, tailY) = (positionsTailVisited, (tailX, tailY))
  | headX == tailX || headY == tailY = 
    let
      newTailPosition = moveTailSameRowCol (headX, headY) (tailX, tailY)
      updatedTailPositions = Data.Set.insert newTailPosition positionsTailVisited
    in
    updateTailPosition (headX, headY) newTailPosition updatedTailPositions 
  | otherwise = 
    let
      newTailPosition = moveTailDiagonally (headX, headY) (tailX, tailY)
      updatedTailPositions = Data.Set.insert newTailPosition positionsTailVisited 
    in
    updateTailPosition (headX, headY) newTailPosition updatedTailPositions 

moveTailDiagonally :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTailDiagonally (headX, headY) (tailX, tailY)
  | xDiff >= 1 && yDiff >= 1 = (tailX + 1, tailY + 1)
  | xDiff >= 1 && yDiff <= -1 = (tailX + 1, tailY - 1)
  | xDiff <= -1 && yDiff >= 1 = (tailX - 1, tailY + 1)
  | xDiff <= -1 && yDiff <= -1 = (tailX - 1, tailY - 1)
  | otherwise = trace "ERROR 1" (tailX, tailY)
  where
    xDiff = headX - tailX
    yDiff = headY - tailY

moveTailSameRowCol :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTailSameRowCol (headX, headY) (tailX, tailY)
  | tailY < headY = (tailX, tailY + 1)
  | tailY > headY = (tailX, tailY - 1)
  | tailX < headX = (tailX + 1, tailY)
  | tailX > headX = (tailX - 1, tailY)
  | otherwise = trace "ERROR 2" (tailX, tailY)

makeMove :: String -> (Int, Int) -> (Int, Int)
makeMove theMove (x, y)
  | direction == "R" = (x + amount, y)
  | direction == "L" = (x - amount, y) 
  | direction == "U" = (x, y + amount) 
  | direction == "D" = (x, y - amount)
  | otherwise = trace "ERROR 3" (x, y)
  where
    processedMove = processMove theMove
    direction = fst processedMove
    amount = snd processedMove

processMove :: String -> (String, Int)
processMove theMove = (direction, amount)
  where
    theMoveLst = words theMove
    direction = head theMoveLst
    amount = read (last theMoveLst) :: Int

-- Number of positions tail visited: 5710
-- Number of positions tail visited (w/ more knots): 2259