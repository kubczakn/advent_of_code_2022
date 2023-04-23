import Data.List
import Data.Maybe
import Data.Map
import Data.Set
import Debug.Trace

{-

* Figure out signal sent by CPU
* CPU has only a single register, which starts with the value 1
* Supports two instructions:
    addx V takes two cycles, adds value V to X
    noop takes one cycle, has no effects
* Signal strength is the cycle number multiplied by the X
    value of the register
* Want to look at 20th cycle and every 40th cycle following
    (so 20th, 60th, 100th, 140th, 180th, and 220th)

-}

{-

* X register stores horizontal position of a sprite
* Sprite three pixels wide and X register sets horizontal
    position of the middle of that sprite
* 
* If sprite is positioned such that it is one of the three
    pixels being drawn, the screen produces a lit pixel
* Render the image given by your program. What eight capital letters
    appear?

-}

-- Store strength of each cycle in a map

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- contents <- readFile "sample-input.txt"
  let cyclesMap = applyInstructions (lines contents) 1 1 (Data.Map.fromList [(1, 1)])
  -- print cyclesMap 
  -- print (sumTaskOneStrengths cyclesMap 20)
  putStr (drawTaskTwo cyclesMap 1 0)

applyInstructions :: [String] -> Int -> Int -> Map Int Int -> Map Int Int
applyInstructions [] _ _ cycleStengths = cycleStengths
applyInstructions (currentInstruction:otherInstructions) currentCycle currentStrength cycleStrengths 
  | instruction == "noop" 
    = let
        nextCycle = currentCycle + 1
      in
      applyInstructions otherInstructions nextCycle currentStrength 
      (Data.Map.insert nextCycle currentStrength cycleStrengths) 
  | instruction == "addx"
    = let
        strengthsAfterFirstAddCycle = Data.Map.insert (currentCycle + 1) currentStrength cycleStrengths
        instructionValue = read (last instructionText) :: Int
        nextCycle = currentCycle + 2
        newStrength = currentStrength + instructionValue
      in
        applyInstructions otherInstructions nextCycle newStrength 
        (Data.Map.insert nextCycle newStrength strengthsAfterFirstAddCycle)
  | otherwise = trace "ERROR 1" cycleStrengths
  where
    instructionText = words currentInstruction
    instruction = head instructionText 

sumTaskOneStrengths :: Map Int Int -> Int -> Int
sumTaskOneStrengths cycleStrengths currentCycle
  | currentCycle > Data.Map.size cycleStrengths = 0
  | otherwise = getSignalStrength cycleStrengths currentCycle + sumTaskOneStrengths cycleStrengths (currentCycle + 40)

drawTaskTwo :: Map Int Int -> Int -> Int -> String 
drawTaskTwo cycleStrengths currentCycle currentPixel 
  | currentCycle > Data.Map.size cycleStrengths = "" 
  | isCurrentPixelLit currentPixel currentRegisterValue = 
    addPixel '#' currentPixel ++ drawTaskTwo cycleStrengths (currentCycle + 1) nextPixel
  | otherwise = 
    addPixel '.' currentPixel ++ drawTaskTwo cycleStrengths (currentCycle + 1) nextPixel 
  where
    currentRegisterValue = fromJust (Data.Map.lookup currentCycle cycleStrengths)
    nextPixel = (currentPixel + 1) `mod` 40

addPixel :: Char -> Int -> String
addPixel character currentPixel 
  | (currentPixel + 1) == 40 = character : "\n" 
  | otherwise = [character]

isCurrentPixelLit :: Int -> Int -> Bool
isCurrentPixelLit currentPixel registerValue = pixelDiff <= 1 && pixelDiff >= -1
  where
    pixelDiff = registerValue - currentPixel 

getSignalStrength :: Map Int Int -> Int -> Int
getSignalStrength cycleStrengths theCycle =
  cycleValue * theCycle
  where
    cycleValue = fromJust (Data.Map.lookup theCycle cycleStrengths)

-- Task one cycle strength sum: 14806
-- Task two captial letters are: RGZEHURK