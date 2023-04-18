import Data.List
import Data.Maybe
import Data.Map
import Debug.Trace

{-

* Find all directories with a total size at most 100000.
* Find the sum of the sizes of these directories

-}

{-

* Need to find the smallest directory, that if deleted, would free up enough
  space on the file system to run the update.
* Total space available is 70,000,000
* Need unused space of at least 30,000,000

-}

data Directory = 
  Directory
  { 
    total_size :: Int, 
    directories :: [String] 
  }
  deriving (Show)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  -- contents <- readFile "sample-input.txt"
  let parsedInput = removeDuplicateNames (lines contents) empty 
  let fileTree = fromList [("/", Directory{total_size = 0, directories = []})] 
  let firstPassFileTree = createFileTree parsedInput "/" fileTree
  let secondPassFileTree = updateDirectorySizeFromChildren "/" firstPassFileTree
  let fileSystemSize = total_size (fromJust (Data.Map.lookup "/" secondPassFileTree))
  let currentUnusedSpace = 70000000 - fileSystemSize
  -- print parsedInput
  -- print firstPassFileTree
  print secondPassFileTree
  -- print (findSumOfDirectoriesWithSize (Data.Map.toList secondPassFileTree))
  -- print fileSystemSize
  -- print currentUnusedSpace
  print (findSizeOfDirectoryToDelete currentUnusedSpace fileSystemSize (Data.Map.toList secondPassFileTree))

findSizeOfDirectoryToDelete :: Int -> Int -> [(String, Directory)] -> Int
findSizeOfDirectoryToDelete _ targetDirectorySize [] = targetDirectorySize
findSizeOfDirectoryToDelete unusedSpace targetDirectorySize (x:xs)
  | currentDirectorySize <= targetDirectorySize && unusedSpaceAfterDeletion >= 30000000 =
    trace ("DEBUG: " ++ show (fst x) ++ " " ++ show currentDirectorySize ++ " " ++ show unusedSpaceAfterDeletion) findSizeOfDirectoryToDelete unusedSpace currentDirectorySize xs
  | otherwise = findSizeOfDirectoryToDelete unusedSpace targetDirectorySize xs
  where
    currentDirectorySize = total_size (snd x) 
    unusedSpaceAfterDeletion = unusedSpace + currentDirectorySize

removeDuplicateNames :: [String] -> Map String Int -> [String]
removeDuplicateNames [] _ = []
removeDuplicateNames (x:xs) nameMap
  | "$ cd .." == x = x : removeDuplicateNames xs nameMap
  | firstThreeInputLetters == "dir" = 
    let updatedMap = updateNameMap x nameMap 
    in updateInputString x updatedMap : removeDuplicateNames xs updatedMap
  | "$ cd" `isInfixOf` x = updateInputString x nameMap : removeDuplicateNames xs nameMap 
  | otherwise = x : removeDuplicateNames xs nameMap
  where
    inputWords = words x
    firstThreeInputLetters = Data.List.take 3 x

updateNameMap :: String -> Map String Int -> Map String Int
updateNameMap input nameMap = case Data.Map.lookup name nameMap of
  Just id ->  Data.Map.insert name (id + 1) nameMap
  Nothing -> Data.Map.insert name 0 nameMap
  where
    name = last (words input)
  
updateInputString :: String -> Map String Int -> String -- assumes map already updated
updateInputString input nameMap = case Data.Map.lookup name nameMap of
  Just id -> input ++ show id
  Nothing -> input
  where
    name = last (words input)

findSumOfDirectoriesWithSize :: [(String, Directory)] -> Int
findSumOfDirectoriesWithSize [] = 0
findSumOfDirectoriesWithSize (currentDirectory : otherDirectories) 
  | directorySize <= 100000 = directorySize + findSumOfDirectoriesWithSize otherDirectories
  | otherwise = findSumOfDirectoriesWithSize otherDirectories
  where
    directorySize = total_size (snd currentDirectory)

updateDirectorySizeFromChildren :: String -> Map String Directory -> Map String Directory
updateDirectorySizeFromChildren directoryName fileTree = newFileTree 
  where
    directory = fromJust (Data.Map.lookup directoryName fileTree)
    childDirectories = directories directory
    currentSize = total_size directory
    updatedChildSizesFileTree = findChildSizes childDirectories fileTree
    childSizeSum = sumChildSizes childDirectories updatedChildSizesFileTree 
    updatedDirectory = directory {total_size = currentSize + childSizeSum}
    newFileTree = Data.Map.insert directoryName updatedDirectory updatedChildSizesFileTree 

findChildSizes :: [String] -> Map String Directory -> Map String Directory 
findChildSizes [] fileTree = fileTree -- base case
findChildSizes (child:otherChildren) fileTree = findChildSizes otherChildren updatedFileTree
  where
    updatedFileTree = updateDirectorySizeFromChildren child fileTree

sumChildSizes :: [String] -> Map String Directory -> Int
sumChildSizes [] _ = 0
sumChildSizes (child:otherChildren) fileTree = childSize + sumChildSizes otherChildren fileTree
  where
    childDirectory = fromJust (Data.Map.lookup child fileTree)
    childSize = total_size childDirectory

createFileTree :: [String] -> String -> Map String Directory -> Map String Directory
createFileTree [] _ currFileTree = currFileTree
createFileTree (x:xs) currDirectoryName currFileTree 
  | "$ cd .." == x || "$ ls" == x = createFileTree xs currDirectoryName currFileTree
  | firstThreeInputLetters == "dir" = createFileTree xs currDirectoryName (createNewDirectory (last inputWords) currDirectoryName currFileTree) 
  | "$ cd" `isInfixOf`x =  createFileTree xs (last inputWords) currFileTree
  | otherwise = 
    createFileTree xs currDirectoryName (updateDirectorySize (read (head inputWords) :: Int) currDirectoryName currFileTree)
  where 
    inputWords = words x
    firstThreeInputLetters = Data.List.take 3 x

createNewDirectory :: String -> String -> Map String Directory -> Map String Directory
createNewDirectory newDirectoryName parentDirectoryName fileTree = 
  Data.Map.insert parentDirectoryName updatedParentDirectory updatedFileTree
  where
    parentDirectory = fromJust (Data.Map.lookup parentDirectoryName fileTree)
    updatedParentDirectory = addDirectoryToParent newDirectoryName parentDirectory
    updatedFileTree = addDirectoryToFileTree newDirectoryName fileTree

addDirectoryToParent :: String -> Directory -> Directory
addDirectoryToParent newDirectoryName parentDirectory =
  parentDirectory {directories=newChildren} 
  where 
    newChildren = newDirectoryName : directories parentDirectory

addDirectoryToFileTree :: String -> Map String Directory -> Map String Directory
addDirectoryToFileTree directoryName = 
  Data.Map.insert directoryName Directory { total_size=0, directories=[] } 

updateDirectorySize :: Int -> String -> Map String Directory -> Map String Directory
updateDirectorySize sizeToAdd directoryName fileTree = 
  Data.Map.insert directoryName updatedDirectory fileTree
  where
    currDirectory = fromJust (Data.Map.lookup directoryName fileTree)
    currDirectorySize = total_size currDirectory
    updatedDirectory = currDirectory { total_size = currDirectorySize + sizeToAdd }

-- Total size of all directories at most 100,000 is 1743217