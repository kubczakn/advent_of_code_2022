import Data.List
import Data.Tuple
import Data.Bifunctor
import Data.Maybe
import Data.Set
import Data.Char
import Distribution.Compat.CharParsing (upper)

{-

* Each line represents a rucksack with two compartments.
* Each rucksack has a item mismatch in each compartment
  i.e an item shows up in both compartments when it
  should only show up in one.
* Each item also has a certain priority.
* Find the sum of priorities for each rucksack's mismatch.

-}

{-

* Elves divided into groups of three, each set of three input lines
  a group.
* Group badge is the item that is common between each elf in a group.
* Sum the priorities of the badges in each group.

-}

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let rucksacks = lines contents
  -- let compartments = Data.List.map getRucksackCompartments rucksacks
  -- let mismatches = Data.List.map (fromJust . findMismatch) compartments
  -- print (sumPriorities mismatches)
  let badges = getBadges rucksacks
  print (sumPriorities badges)

getRucksackCompartments :: String -> (String, String)
getRucksackCompartments = getCompartmentsHelper 0 
  where
    getCompartmentsHelper :: Int -> String -> (String, String)
    getCompartmentsHelper count (x:xs)
      | count >= length xs = ([], x : xs)
      | otherwise = first (x :) (getCompartmentsHelper (count + 1) xs)

findMismatch :: (String, String) -> Maybe Char
findMismatch (firstCompartment, secondCompartment) = 
  findMismatchHelper (fromList firstCompartment) secondCompartment 
  where
    findMismatchHelper :: Set Char -> String -> Maybe Char
    findMismatchHelper firstCompartmentSet (x:xs)
      | member x firstCompartmentSet = Just x 
      | otherwise = findMismatchHelper firstCompartmentSet xs
    findMismatchHelper _ [] = Nothing

getBadges :: [String] -> String
getBadges [] = [] 
getBadges (first:second:third:rest) = findBadge first second third : getBadges rest

findBadge :: String -> String -> String -> Char
findBadge rucksackOne rucksackTwo rucksackThree = head (elems rucksackIntersection)
  where
    rucksackOneSet = fromList rucksackOne
    rucksackTwoSet = fromList rucksackTwo
    rucksackThreeSet = fromList rucksackThree
    rucksackIntersection = intersection rucksackOneSet (intersection rucksackTwoSet rucksackThreeSet)

sumPriorities :: String -> Int
sumPriorities [] = 0
sumPriorities (x:xs) 
  | isUpper x = (ord x - upperAsciiOffset) + sumPriorities xs
  | otherwise = (ord x - lowerAsciiOffset) + sumPriorities xs
  where
    upperAsciiOffset = 38
    lowerAsciiOffset = 96


-- Sum of mismatches is 8176
-- Sum of badges is 2689
