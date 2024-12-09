import Data.List.Split

redCubes = 12 :: Int
greenCubes = 13 :: Int
blueCubes = 14 :: Int

-- main :: IO ()  -- por convención, no se suele especificar una declaración de tipo para main
main = do
  contents <- getContents  -- theoretically (program level) reads all lines until an EOF. In practice, it reads line by line because of lazyness
  {--
  -- First part of the challenge
  let possibleGamesNumbers = possibleGames contents
  -- print $ possibleGamesNumbers
  print $ sum possibleGamesNumbers
  --}
  {--}
  -- Second part of the challenge
  let minimumBallsNumbers = getMinimumBallsNumbers contents
  print $ minimumBallsNumbers
  print $ sum (map product minimumBallsNumbers)
  --}

possibleGames :: String -> [Int]
possibleGames input =
  let allLines = lines input
      possibles = map possibleGame allLines
  in possibles

possibleGame :: String -> Int
possibleGame game
  | and $ map setIsPossible sets = read gameNumber :: Int
  | otherwise = 0
  where
    [gameAndNumber, allSets] = splitOn ":" game
    sets = splitOn ";" allSets
    [_, gameNumber] = words gameAndNumber

setIsPossible :: String -> Bool
setIsPossible set = and $ map takeIsPossible takes
  where
    takes = splitOn "," set

takeIsPossible :: String -> Bool
takeIsPossible take = case color of
                        "red" -> amount <= redCubes
                        "green" -> amount <= greenCubes
                        "blue" -> amount <= blueCubes
  where
    [amountString, color] = words take
    amount = read amountString :: Int

getMinimumBallsNumbers :: String -> [[Int]]
getMinimumBallsNumbers input = map getMinimumNumbersGame allLines
  where allLines = lines input

getMinimumNumbersGame :: String -> [Int]
getMinimumNumbersGame game = map maximum [map (getColorNumbersSet "red") sets, map (getColorNumbersSet "green") sets, map (getColorNumbersSet "blue") sets]
  where
    [gameAndNumber, allSets] = splitOn ":" game
    sets = splitOn ";" allSets

getColorNumbersSet :: String -> String -> Int
getColorNumbersSet color set = maximum (map (getColorNumberTake color) takes)
  where
    takes = splitOn "," set

getColorNumberTake :: String -> String -> Int
getColorNumberTake color take = number
  where
    [amountString, colorString] = words take
    number = if colorString == color then read amountString :: Int else 0


shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in  result