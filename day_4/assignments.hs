import Data.List
import Data.Tuple
import Data.Bifunctor

{-

* Each elf is assigned a section to clean.
* Many section assignments overlap with each other.
* Elves have paired up to combine their section assignments
    to find overlaps.
* Each assignment pair is a line containing two range of x-y.
* Find the number of assignment pairs where one range
    fully contains the other.

-}

{-

Now find the number of pairs that overlap at all!

-}

main :: IO ()
main = do
  -- contents <- readFile "sample-input.txt"
  contents <- readFile "input.txt"
  let parsedRanges = map (splitByChar ',') (lines contents) 
  print (sumPairOverlaps parsedRanges checkRangeOverlap) 

sumPairOverlaps :: [(String, String)] -> ((Int, Int) -> (Int, Int) -> Bool) -> Int
sumPairOverlaps [] _ = 0
sumPairOverlaps (x:xs) overlapPredicate
  | isOverlapInPair x overlapPredicate = 1 + sumPairOverlaps xs overlapPredicate
  | otherwise = sumPairOverlaps xs overlapPredicate

isOverlapInPair :: (String, String) -> ((Int, Int) -> (Int, Int) -> Bool) -> Bool
isOverlapInPair rangePair overlapPredicate = rangeOneOverlapsTwo || rangeTwoOverlapsOne 
  where
    convertedRangeOne = parseSectionRange (fst rangePair)
    convertedRangeTwo = parseSectionRange (snd rangePair)
    rangeOneOverlapsTwo = overlapPredicate convertedRangeOne convertedRangeTwo 
    rangeTwoOverlapsOne = overlapPredicate convertedRangeTwo convertedRangeOne

checkCompleteRangeOverlap :: (Int, Int) -> (Int, Int) -> Bool
checkCompleteRangeOverlap (startOne, endOne) (startTwo, endTwo) = 
  startOne <= startTwo && endOne >= endTwo

checkRangeOverlap :: (Int, Int) -> (Int, Int) -> Bool
checkRangeOverlap (startOne, endOne) (startTwo, endTwo) = 
  endOne >= startTwo && startOne <= endTwo

parseSectionRange :: String -> (Int, Int)
parseSectionRange rangeStr = (startRange, endRange)
  where
    tupleRangeStr = splitByChar '-' rangeStr
    startRange = read (fst tupleRangeStr) :: Int
    endRange = read (snd tupleRangeStr) :: Int

splitByChar :: Char -> String -> (String, String)
splitByChar _ [] = ([], [])
splitByChar c (x:xs)
  | x == c = ([], xs)
  | otherwise = first (x :) (splitByChar c xs)

-- There are 494 assignment pairs where one range fully contains the other
-- There are 833 assignment pairs that overlap