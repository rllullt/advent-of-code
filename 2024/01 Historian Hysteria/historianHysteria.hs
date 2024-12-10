import Data.List


{-- First part
--
-- main :: IO ()
main = do
  contents <- getContents
  let distances = getDistances contents
      totalDistance = sum distances
  print $ totalDistance
--}

getDistances :: String -> [Int]
getDistances input =
  let allLines = getAllLines input
      sortedLeftAndRightLists = sepLists [] [] (map (\ls -> ((ls !! 0), (ls !! 1))) allLines)
      (left, right) = (sortedLeftAndRightLists !! 0, sortedLeftAndRightLists !! 1)
      distances = zipWith (\a b -> abs $ a - b) left right
  in distances

getAllLines :: String -> [[String]]
getAllLines = map words . lines

sepLists :: [Int] -> [Int] -> [(String, String)] -> [[Int]]
sepLists left right [] = [sort left, sort right]
sepLists left right ((s1, s2) : ls) = sepLists ((read s1 :: Int) : left) ((read s2 :: Int) : right) ls


{-- Second part
--}
main = do
  contents <- getContents
  let allLines = getAllLines contents
      sortedLeftAndRightLists = sepLists [] [] (map (\ls -> ((ls !! 0), (ls !! 1))) allLines)
      (left, right) = (sortedLeftAndRightLists !! 0, sortedLeftAndRightLists !! 1)
      numberOfLeftsInRight = map (\x -> count x right) left
      leftsTimesApparitionsInRight = zipWith (*) left numberOfLeftsInRight
      similarityScore = sum leftsTimesApparitionsInRight
  print $ similarityScore
--}

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)