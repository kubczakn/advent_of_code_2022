import Data.List
import Data.Maybe

{-

Part 1:

First column: A for rock, B for paper, C for scissors 
Second column: X for rock, Y for paper, Z for scissors 

Total score: 
  1 for rock, 2 for scissors, 3 for paper + 0 for loss, 6 for win

Goal: Find total score following this strategy

-}

{-

Part 2: 

Second column: X means you need to win, Y means it ends in a draw, Z means you have to lose

-}

data Move = Rock | Paper | Scissors 
  deriving (Show, Eq)

data Outcome = Win | Draw | Loss 
  deriving (Show, Eq)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- print (take 1 (parseInput contents))
  print (sumRounds (parseInput contents))

parseInput :: String -> [[Move]]
parseInput input = roundList 
  where
    stringList = map words (lines input)
    -- roundMaybes = map (map stringToMoveStrategyOne) stringList
    -- roundList = map (map fromJust) roundMaybes
    roundList = map stringToMoveStrategyTwo stringList
  
sumRounds  :: [[Move]] -> Int
sumRounds [] = 0
sumRounds (x:xs) = case x of
  (theirMove : yourMove : _) -> calculateRoundScore theirMove yourMove + sumRounds xs
  _ -> sumRounds xs

stringToOutcome :: String -> Maybe Outcome 
stringToOutcome str 
  | str == "X" = Just Loss 
  | str == "Y" = Just Draw 
  | str == "Z" = Just Win 
  | otherwise = Nothing

stringToMoveStrategyOne :: String -> Maybe Move
stringToMoveStrategyOne str 
  | str == "A" || str == "X" = Just Rock
  | str == "B" || str == "Y" = Just Paper 
  | str == "C" || str == "Z" = Just Scissors 
  | otherwise = Nothing

stringToMoveStrategyTwo :: [String] -> [Move]
stringToMoveStrategyTwo (opponentMoveString:outcomeString:_) = [opponentMove, yourMove]
  where
    opponentMove = fromJust (stringToMoveStrategyOne opponentMoveString)
    theOutcome = fromJust (stringToOutcome outcomeString)
    yourMove = outcomeToMove opponentMove theOutcome

calculateRoundScore :: Move -> Move -> Int
calculateRoundScore opponentMove yourMove 
  | isRoundWin opponentMove yourMove = 6 + moveValue yourMove 
  | opponentMove == yourMove = 3 + moveValue yourMove
  | otherwise = moveValue yourMove

isRoundWin :: Move -> Move -> Bool
isRoundWin opponentMove yourMove = case yourMove of 
  Rock -> opponentMove == Scissors
  Scissors -> opponentMove == Paper
  Paper -> opponentMove == Rock

moveValue :: Move -> Int
moveValue Rock = 1
moveValue Paper = 2
moveValue Scissors = 3

outcomeToMove :: Move ->  Outcome -> Move 
outcomeToMove Rock outcome = case outcome of
  Win -> Paper
  Draw -> Rock
  Loss -> Scissors
outcomeToMove Paper outcome = case outcome of
  Win -> Scissors 
  Draw -> Paper 
  Loss -> Rock 
outcomeToMove Scissors outcome = case outcome of
  Win -> Rock 
  Draw -> Scissors 
  Loss -> Paper 

-- Score over all rounds was 14069
