{-- First part
--}
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
