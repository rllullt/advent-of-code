import Data.Set (fromList, toList)

main = do
  contents <- getContents
  let theMap = lines contents
      antinodes = getAntinodes theMap
  print theMap
  print antinodes
  print $ length antinodes

getAntinodes :: [[Char]] -> [(Int, Int)]
getAntinodes [] = []
getAntinodes xs = 
  let m = length xs
      n = length (xs!!0)
      upper = [[(i - abs (i-k), j - abs (j-l)) | k <- [i..m-1], l <- [0..n-1], xs !! k !! l == xs !! i !! j, 0 <= i - abs (i-k) && i - abs (i-k) < m, 0 <= j - abs (j-l) && j - abs (j-l) < n] | i <- [0..m-1], j <- [0..n-1], xs !! i !! j /= '.']
      upperAntinodes = extractAntinodes upper
      lower = [[(k + abs (i-k), l + abs (j-l)) | k <- [i..m-1], l <- [0..n-1], xs !! k !! l == xs !! i !! j, 0 <= k + abs (i-k) && k + abs (i-k) < m, 0 <= l + abs (j-l) && l + abs (j-l) < n] | i <- [0..m-1], j <- [0..n-1], xs !! i !! j /= '.']
      lowerAntinodes = extractAntinodes lower
  in removeDuplicates $ lowerAntinodes ++ upperAntinodes
  where extractAntinodes = foldl (\l ps -> l ++ tail ps) [] . filter (\ps -> length ps > 1)

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = toList . fromList
      