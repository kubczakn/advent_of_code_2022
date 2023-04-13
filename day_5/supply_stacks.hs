import Data.List
import Data.Tuple
import Data.Map 
import Data.Maybe

{-

* Given a crate arrangement and a set of move procedures
    between different create stacks.
* Need to find what crates end up on the top of each stack

-}

{-

* Part 2 is the same idea except crates aren't moved one at a time (i.e order is preserved)

-}


main :: IO ()
main = do
  -- contents <- readFile "sample-input.txt"
  contents <- readFile "input.txt"
  let input = addSpaceToStackRows (lines contents)
  let initialMap = parseStackRows input (initMap (head input))
  let moveInput = getMoveInput input
  let moves = parseMoves moveInput 
  print (getStackTops (applyMoves moves initialMap))

getStackTops :: Map Int String -> String
getStackTops theMap = getStackTopsHelper theMap 1
  where 
    getStackTopsHelper :: Map Int String -> Int -> String
    getStackTopsHelper theMap currStack
      | currStack > size theMap = []
      | otherwise = Data.List.take 1 (Data.Maybe.fromJust (Data.Map.lookup currStack theMap)) ++ getStackTopsHelper theMap (currStack + 1)

applyMoves :: [[Int]] -> Map Int String -> Map Int String
applyMoves [] currMap = currMap
applyMoves ((amount : start : end : other):xs) currMap = applyMoves xs (applyMove start end amount currMap)

applyMove :: Int -> Int -> Int -> Map Int String -> Map Int String
applyMove startStackIdx endStackIdx amount currMap =
  Data.Map.insert endStackIdx newEndStack (Data.Map.insert startStackIdx newStartStack currMap) 
  where
    startStack = Data.Maybe.fromJust (Data.Map.lookup startStackIdx currMap)
    endStack = Data.Maybe.fromJust (Data.Map.lookup endStackIdx currMap)
    cratesToMove = Data.List.take amount startStack
    newStartStack = Data.List.drop amount startStack
    -- newEndStack = reverse cratesToMove ++ endStack
    newEndStack = cratesToMove ++ endStack

getMoveInput :: [String] -> [String]
getMoveInput [] = []
getMoveInput (x:xs) 
  | x == "" = xs
  | otherwise = getMoveInput xs

parseMoves :: [String] -> [[Int]]
parseMoves [] = []
parseMoves ((head:rest):xs) 
  | head == 'm' = parseMove (head : rest) : parseMoves xs
  | otherwise = parseMoves xs

parseMove :: String -> [Int] 
parseMove inputLine = parseMoveHelper (words inputLine) 1
  where
    parseMoveHelper :: [String] -> Int -> [Int]
    parseMoveHelper [] _ = []
    parseMoveHelper (x:xs) currWord
      | currWord == 2 || currWord == 4 || currWord == 6 = 
        (read x :: Int) : parseMoveHelper xs (currWord + 1)
      | otherwise = parseMoveHelper xs (currWord + 1)

initMap :: String -> Map Int String
initMap row = initMapHelper row Data.Map.empty 1
  where
    initMapHelper :: String -> Map Int String -> Int -> Map Int String
    initMapHelper [] currMap _ = currMap
    initMapHelper (x:y:z:_:rest) currMap currColumn  = Data.Map.insert currColumn [] builtMap
      where 
        builtMap = initMapHelper rest currMap (currColumn + 1) 

addSpaceToStackRows :: [String] -> [String]
addSpaceToStackRows [] = []
addSpaceToStackRows (x:xs) 
  | x == "" = x : xs
  | otherwise = (x ++ [' ']) : addSpaceToStackRows xs

parseStackRows :: [String] -> Map Int String -> Map Int String
parseStackRows [] currMap = currMap
parseStackRows ((x:y:rest):xs) currMap 
  | y == '1' = currMap 
  | otherwise = parseStackRows xs (parseStackRow currRow currMap) 
  where
    currRow = x : y : rest

parseStackRow :: String -> Map Int String -> Map Int String
parseStackRow row currMap = parseStackRowHelper row currMap 1 
  where
    parseStackRowHelper :: String -> Map Int String -> Int -> Map Int String
    parseStackRowHelper (x:y:z:_:rest) currMap currColumn
      | x == '[' && z == ']' = Data.Map.insert currColumn (currColumnList ++ [y]) builtMap
      | otherwise = builtMap
      where 
        builtMap = parseStackRowHelper rest currMap (currColumn + 1)
        currColumnList = Data.Maybe.fromJust (Data.Map.lookup currColumn builtMap)
    parseStackRowHelper _ currMap _ = currMap

-- Stack tops after all moves: TDCHVHJTG
-- Stack tops after all moves (part 2): NGCMPJLHV
