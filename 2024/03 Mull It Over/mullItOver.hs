import Text.Regex.TDFA

{-- First part
--}
main :: IO ()
main = do
  contents <- getContents
  let allMemories = getAllMemories contents
      allMulInstances = map extractMulInstances allMemories
      allPairs = map extractPairs allMulInstances
      allMults = map extractMults allPairs
      allSums = map (\l -> sum l) allMults
      finalSum = sum allSums
  print $ finalSum
  -- let text = ""
  -- let results = extractMulInstances text
  -- print results

getAllMemories :: String -> [String]
getAllMemories = lines

-- Extracats all "mul(d,d)" from a text into a list ["mul(d,d)", ...]
extractMulInstances :: String -> [String]
extractMulInstances text =
  let pat = "mul\\([0-9]{1,3},[0-9]{1,3}\\)" -- RegExp
  in getAllTextMatches (text =~ pat :: AllTextMatches [] String)  -- =~ applies a regexp to a text, the [] String cast is for obtaining all the coincidences


-- Extracts all pair of numbers from a list ["mul(d,d)" ...] into a list [(d,d), ...]
extractPairs :: [String] -> [(Int, Int)]
extractPairs = map getPairInString
  where getPairInString = \s -> read (getPair s) :: (Int, Int)
        getPair = \s -> head (getAllTextMatches (s =~ pat :: AllTextMatches [] String))
        pat = "\\([0-9]+,[0-9]+\\)"

extractMults :: [(Int, Int)] -> [Int]
extractMults = map (\(x,y) -> x*y)
