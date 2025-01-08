import Data.List.Split

{-- First part
--
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
      defragmentedDisk = defragmentDiskEfficient (length disk - 1) 0 disk
      t = tail $ splitOn "" "00...111...2...333.44....."
  print $ disk
  print $ defragmentedDisk

defragmentDiskEfficient :: Int -> Int -> [String] -> [String]
defragmentDiskEfficient i j disk = if i <= 0
                                   then disk
                                   else  if disk !! i /= "."
                                         then let (iniF, endF) = getIniIndex (i-1) i disk
                                                  (iniE, endE) = getEndIndex j (j+1) disk
                                                  fileWidth = endF - iniF + 1
                                                  emptyWidth = endE - iniE + 1
                                              in if emptyWidth >= fileWidth && endE < iniF
                                                 then let newDisk = writeDisk iniE endE iniF endF (disk !! i) disk
                                                      in defragmentDiskEfficient (length disk - 1) 0 newDisk
                                                 else defragmentDiskEfficient (i-fileWidth) 0 disk
                                         else defragmentDiskEfficient (i-1) 0 disk

getIniIndex 0 end disk = (0, end)
getIniIndex ini end disk = if disk !! ini == disk !! end
                           then getIniIndex (ini-1) end disk
                           else (ini+1, end)

getEndIndex ini end disk = if disk !! ini /= "."
                           then getEndIndex (ini+1) (end+1) disk
                           else if end < length disk && disk !! ini == disk !! end
                                then getEndIndex ini (end+1) disk
                                else (ini, end-1)

writeDisk iniE endE iniF endF a disk = map (\i -> if iniE <= i && i <= endE && i - fileWidth < iniE
                                                  then a
                                                  else if iniF <= i && i <= endF
                                                       then "."
                                                       else disk !! i)
                                           [0..length disk-1]
  where fileWidth = endF - iniF + 1