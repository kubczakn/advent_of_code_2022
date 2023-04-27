import Data.List
import Data.Maybe
import Data.Map
import Debug.Trace

{-

* Monkeys take turns inspecting items.
* Operating shows how worry level changes as a 
    monkey inspects an item.
* After an operation, the monkey grows more bored
    with an item, dividing its worry level by 
    three and rounding to the nearest integer.
* Test determines what the monkey does with an
    item depending on the worry level
* Find the level of monkey business (i.e multiplying)
    the inspection counts of the two monkeys that
    performed the most inspections.
* Find the level of monkey business after 20 rounds

-}

newtype Item = Item { worryLevel :: Int } deriving Show

data Monkey = Monkey {
  numInspections :: Int,
  items :: [Item],
  operation :: Item -> Int -> Item,
  test :: Item -> Int -> Int, -- Monkey to throw to based on test 
  modulo :: Int
}

-- TODO: Deal with integer overflow? Mod with LCM of all tests!

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- contents <- readFile "sample-input.txt"
  let splitMonkeyInput = splitByBlankLine (Data.List.map (\x -> x ++ "\n") (lines contents)) []
  let monkeysMap = initMonkeysMap (Data.List.map parseMonkeyInput splitMonkeyInput) 0
  -- let stateAfterAllRounds = executeRounds 20 monkeysMap 
  let stateAfterAllRounds = executeRounds 10000 monkeysMap 
  let sortedInspectionCounts = reverse (sort (getInspectionCounts 0 stateAfterAllRounds))
  print sortedInspectionCounts
  print (getMonkeyBusiness sortedInspectionCounts)

getMonkeyBusiness :: [Int] -> Int
getMonkeyBusiness (x:y:xs) = x * y

getInspectionCounts :: Int -> Map Int Monkey -> [Int]
getInspectionCounts currentMonkeyId monkeysMap 
  | currentMonkeyId >= (Data.Map.size monkeysMap) = []
  | otherwise = inspectionCount : getInspectionCounts (currentMonkeyId + 1) monkeysMap
    where
      currentMonkey = fromJust (Data.Map.lookup currentMonkeyId monkeysMap)
      inspectionCount = numInspections currentMonkey

showEachMonkeyItems :: [(Int, Monkey)] -> [(Int, [Item])]
showEachMonkeyItems [] = []
showEachMonkeyItems ((currentMonkeyId, currentMonkey) : otherMonkeys) = 
  (currentMonkeyId, monkeyItems) : showEachMonkeyItems otherMonkeys 
  where
    monkeyItems = items currentMonkey

executeRounds :: Int -> Map Int Monkey -> Map Int Monkey
executeRounds roundsLeft monkeyMap
  | roundsLeft == 0 = monkeyMap
  | otherwise = executeRounds (roundsLeft - 1) (runRound 0 monkeyMap)

runRound :: Int -> Map Int Monkey -> Map Int Monkey
runRound currentMonkeyId currentMap
  | currentMonkeyId >= (Data.Map.size currentMap) = currentMap 
  | otherwise = runRound (currentMonkeyId + 1) mapAfterRound
  where 
    currentMonkey = fromJust (Data.Map.lookup currentMonkeyId currentMap)
    currentMonkeyItems = items currentMonkey 
    currentMonkeyInspectionCount = numInspections currentMonkey
    currentMonkeyNoItems = currentMonkey { 
      numInspections = currentMonkeyInspectionCount + (Data.List.length currentMonkeyItems),
      items=[] 
    }
    mapAfterRound = 
      Data.Map.insert currentMonkeyId currentMonkeyNoItems 
      (inspectItem currentMonkeyItems currentMonkey currentMap)

inspectItem :: [Item] -> Monkey -> Map Int Monkey -> Map Int Monkey
inspectItem [] theMonkey monkeyMap = monkeyMap 
inspectItem (currentItem:otherItems) theMonkey monkeyMap = 
  Data.Map.insert monkeyToThrowTo receivingMonkey { items=newItem : receivingMonkeyItems } newMap
  where
    -- newItemWorryLevel = worryLevel ((operation theMonkey) currentItem) 
    -- worryLevelAfterBored = div newItemWorryLevel 3
    -- newItem = Item { worryLevel = worryLevelAfterBored}
    moduloNum = modulo theMonkey
    -- newItem = (operation theMonkey currentItem)
    newItemWorryLevel = worryLevel ((operation theMonkey) currentItem moduloNum)
    newItem = Item{ 
      worryLevel = -- newItemWorryLevel
        if newItemWorryLevel > 9699690 -- 96577 in sample
          then (newItemWorryLevel `mod` 9699690) + (9699690)
          else newItemWorryLevel
      }
    monkeyToThrowTo = 
      -- trace ("Before: " ++ (show currentItem) 
      -- ++ "\nAfter operation: " ++ (show newItemWorryLevel)
      -- ++ "\nAfter division: " ++ (show newItem))
      (test theMonkey) newItem moduloNum
    newMap = (inspectItem otherItems theMonkey monkeyMap) 
    receivingMonkey = fromJust (Data.Map.lookup monkeyToThrowTo newMap)
    receivingMonkeyItems = items receivingMonkey

-- processOperation :: (Item -> Item) -> (String, String, String) _> 

initMonkeysMap :: [Monkey] -> Int -> Map Int Monkey 
initMonkeysMap [] _ = Data.Map.empty
initMonkeysMap (currentMonkey:otherMonkeys) currentId = 
  Data.Map.union 
    (Data.Map.fromList [(currentId, currentMonkey)])
    (initMonkeysMap otherMonkeys (currentId + 1))

parseAllInput :: [String] -> [Monkey]
parseAllInput [] = []
parseAllInput (currentInput:otherInput) = 
  (parseMonkeyInput currentInput) : parseAllInput otherInput

parseMonkeyInput :: String -> Monkey
parseMonkeyInput monkeyInputStr = 
  Monkey{
    numInspections=0,
    items=startingItems,
    operation=operation,
    test=testFunc,
    modulo=read moduloStr :: Int
  }
  where 
    monkeyInput = lines monkeyInputStr
    startingItems = parseStartingItems (last (Data.List.take 2 monkeyInput))
    operation = parseOperation (last (Data.List.take 3 monkeyInput))
    testStr = words (last (Data.List.take 4 monkeyInput))
    trueStr = words (last (Data.List.take 5 monkeyInput))
    falseStr = words (last (Data.List.take 6 monkeyInput))
    moduloStr = (Data.List.last testStr)
    testFunc = parseTest (Data.List.last testStr) (last trueStr) (last falseStr)


parseStartingItems :: String -> [Item]
parseStartingItems [] = trace "Parse starting items error!" []
parseStartingItems (x:xs) 
  | x == ':' = parseStartingItemsHelper xs
  | otherwise = parseStartingItems xs

parseStartingItemsHelper :: String -> [Item]
parseStartingItemsHelper itemsList = convertedItems
  where
    parsedItems = splitByComma itemsList []
    convertedItems = Data.List.map (\x -> Item { worryLevel = read x :: Int}) parsedItems

parseOperation :: String -> (Item -> Int -> Item)
parseOperation [] = trace "Parse operation error!" (\x a -> Item{worryLevel=0})
parseOperation (x:xs) 
  | x == '=' =  parseOperationHelper xs
  | otherwise = parseOperation xs

parseOperationHelper :: String -> (Item -> Int -> Item)
parseOperationHelper operationInput = 
  \x a -> (
    let 
      itemWorryLevel = worryLevel x
      lhs = if leftOperand == "old" then itemWorryLevel else (read leftOperand :: Int)
      operation = if operationStr == "+" then (+) else (*)
      rhs = if rightOperand == "old" then itemWorryLevel else (read rightOperand :: Int)
    in
    Item {worryLevel=(operation lhs rhs)} :: Item)
  where
    operationInputSplit = words operationInput
    leftOperand = head operationInputSplit
    operationStr = last (Data.List.take 2 operationInputSplit)
    rightOperand = last operationInputSplit

parseTest :: String -> String -> String -> (Item -> Int -> Int)
parseTest divisibleByStr monkeyToThrowTrueStr monkeyToThowFalseStr =
  -- trace (show divisibleByStr) 
  (\x a -> (
    let
      itemWorryLevel = worryLevel x
    in
      if itemWorryLevel `mod` a == 0 
      then monkeyToThrowTrue
      else monkeyToThrowFalse
  :: Int))
  where
    monkeyToThrowTrue = read monkeyToThrowTrueStr :: Int
    monkeyToThrowFalse = read monkeyToThowFalseStr :: Int 
    divisibleBy = read divisibleByStr :: Int

splitByBlankLine :: [String] -> String -> [String]
splitByBlankLine [] currentMonkeyInputSequence = [currentMonkeyInputSequence]
splitByBlankLine (x:xs) currentMonkeyInputSequence
  | x == "\n" = currentMonkeyInputSequence : splitByBlankLine xs []
  | otherwise = splitByBlankLine xs (currentMonkeyInputSequence ++ x)

splitByComma :: String -> String -> [String]
splitByComma [] currentItem = [currentItem]
splitByComma (x:xs) currentItem
  | x == ',' = currentItem : splitByComma xs []
  | x == ' ' = splitByComma xs [] 
  | otherwise = splitByComma xs (currentItem ++ [x])

-- Monkey business from task 1: 120384
-- Monkey business from task 2: 32059801242