{-- First part
--
main = do
  contents <- getContents
  let reports = getReports contents
      reportsAreSafe = map reportSafety reports  -- True report is safe, False is not
      numberOfSafeReports = length $ filter (==True) reportsAreSafe
  print $ numberOfSafeReports
--}

getAllLines :: String -> [[String]]
getAllLines = map words . lines

getReports :: String -> [[Int]]
getReports = map (\l -> map read l) . getAllLines

reportSafety :: [Int] -> Bool
reportSafety []           = True
reportSafety (_:[])       = True
reportSafety (x:y:levels) = reportSafetyAux (signum (x - y)) (x:y:levels)

reportSafetyAux :: Int -> [Int] -> Bool
reportSafetyAux s []           = True
reportSafetyAux s (_:[])       = True
reportSafetyAux s (x:y:levels) = 1 <= d && d <= 3 && reportSafetyAux s (y:levels)
  where d = s * (x - y)        


{-- Second part
--}
main = do
  contents <- getContents
  let reports = getReports contents
      reportsAreSafeDampener = map reportSafetyDampener reports
      numberOfSafeReports = length $ filter (==True) reportsAreSafeDampener
  print $ numberOfSafeReports
--}

reportSafetyDampener :: [Int] -> Bool
reportSafetyDampener ls = let allAscOrDescButOne = getAllAscOrDescButOne ls

getAllAscOrDescButOne :: [Int] -> (Bool, Int)
getAllAscOrDescButOne [] = (True, 0)
getAllAscOrDescButOne (_:[]) = (True, 0)
getAllAscOrDescButOne (x:y:ls) = if 1 <= a && a <= 3
                                 then (countAsc, 1+countDesc)
                                 else (1+countAsc, countDesc)(tailAscOrDescButOne && 1 <= d && d <= 3, )
                              
reportSafetyDampener [] = True
reportSafetyDampener (_:[]) = True
reportSafetyDampener (x:y:ls) = snd reportSafetyDampenerAux (signum (x-y)) (x:y:ls)

reportSafetyDampenerAux [] = (True, 1, False)
reportSafetyDampenerAux (_:[]) = (True, 1, False)
reportSafetyDampenerAux (x:y:[]) = let s = signum (x-y)
                                       d = s * (x-y)
                                       elim = not (1 <= d && d <= 3)
                                   in (not elim, s, elim)
-- z cumple la propiedad con ls
reportSafetyDampenerAux (x:y:z:ls) = (safetyTail && safetyHead, s, elimInHead)
  where (safetyTail, s, elimInTail) = reportSafetyDampenerAux (y:z:ls)
        safetyHead = (1 <= d && d <= 3) || not elimInTail
