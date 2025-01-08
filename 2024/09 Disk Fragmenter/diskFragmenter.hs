import Data.List.Split

{-- First part
--}
main = do
  contents <- getContents
  let disk = head $ map getDisk $ lines contents  -- the input is only one disk memory
      defragmentedDisk = defragmentDisk disk
      checksum = calculateChecksum defragmentedDisk
  -- print $ disk
  print $ unwords defragmentedDisk
  print $ checksum
--}

data DiskType = File | Space

getDisk :: String -> [String]
getDisk = markDisk [] 0 File . map (\x -> read x :: Int) . tail . splitOn ""

markDisk :: [String] -> Int -> DiskType -> [Int] -> [String]
markDisk disk _ _ [] = disk
markDisk disk id File (x:xs) = markDisk (appendToDisk (show id) x disk) (id+1) Space xs
markDisk disk id Space (x:xs) = markDisk (appendToDisk "." x disk) id File xs

appendToDisk :: String -> Int -> [String] -> [String]
appendToDisk s x disk = foldl (\d _ -> d ++ [s]) disk [1..x]


-- defragmentDisk = moveFileIds [] . reverse

-- moveFileIds d [] = d
-- moveFileIds d (".":xs) = moveFileIds d xs
-- moveFileIds d (x:xs) = moveFileIds (x : d) xs
defragmentDisk (x:[]) = [x]
defragmentDisk (".":xs) = let y = last xs
                          in if y /= "."
                             then defragmentDisk (init (y:xs))
                             else defragmentDisk (init (".":xs))
defragmentDisk (x:xs) = x : defragmentDisk xs
                           
-- moveToFirstBlank _ [] = []
-- moveToFirstBlank s (".":xs) = s : defragmentDisk xs
-- moveToFirstBlank s (x:xs) = x : moveToFirstBlank s xs

calculateChecksum :: [String] -> Int
calculateChecksum disk = foldl (\a (i, x) -> a + i * (read x :: Int)) 0 (zip [0..] disk)


{-- Second part
--}
main = do
  contents <- getContents
  let disk = head $ map getDisk $ lines contents
  print $ disk

defragmentDiskEfficient i j disk = if disk !! i /= "."
                                   then let (iniF, endF) =
                                            (iniE, endE) =
                                            fileWidth = endF - iniF + 1
                                            emptyWidth = endE - iniE + 1
                                        in if emptyWidth >= fileWidth
                                           then let newDisk = writeFile iniE endE (disk !! i) disk
                                                in defragmentDiskEfficient (iniF-1) (endE+1) newDisk
                                           else defragmentDiskEfficient i (endE+1) disk
                                   else defragmentDiskEfficient (i-1) j disk