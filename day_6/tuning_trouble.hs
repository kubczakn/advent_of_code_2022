import Data.List
import Data.Set

{-

* Given a sequence of characters.
* Need to detect start-of-packet marker, start-of-packet marker
    is a sequence of four characters that are all different.
* Need to figure out how many characters need to be processed before the
    first start-of-packet is detected.

-}

{-

* Part 2 is same idea but markets are 14 characters long.

-}


main :: IO ()
main = do
  -- contents <- readFile "sample-input.txt"
  contents <- readFile "input.txt"
  print (numberOfCharactersToProcess contents)

numberOfCharactersToProcess :: String -> Int
numberOfCharactersToProcess [] = 0
numberOfCharactersToProcess sequence
  -- | isStartMarker (Data.List.take 4 sequence) = 4
  | isStartMarker (Data.List.take 14 sequence) = 14
  | otherwise = 1 + numberOfCharactersToProcess (tail sequence)

isStartMarker :: String -> Bool
isStartMarker chars = Data.Set.size charSet == 14
  where
    charSet = Data.Set.fromList chars 

-- 1794 characters with 4 character start-of-packet markets