import Data.List

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let calorieAmounts = parseCalories contents 
  let aggregatedCalories = map sum calorieAmounts
  -- print (maximum aggregatedCalories)
  let topThreeElves = take 3 (reverse (sort aggregatedCalories))
  print (sum topThreeElves)

parseCalories :: String -> [[Int]]
parseCalories calories = calorieAmounts 
  where 
    splitCalorieStrings = splitByElves (lines calories) []
    calorieAmounts = map (map read) splitCalorieStrings 

splitByElves :: [String] -> [String] -> [[String]]
splitByElves [] _ = [[]]
splitByElves (x:xs) currentElfCalories 
  | x == "" = currentElfCalories : splitByElves xs []
  | otherwise = splitByElves xs (x : currentElfCalories)



-- Elf with most calories had 67622
