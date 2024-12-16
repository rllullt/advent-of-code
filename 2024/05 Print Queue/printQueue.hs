import Prelude hiding (lookup)
import Data.List.Split
import Data.Map (Map, fromList, insert, lookup)

{-- First part
--}
main = do
  contents <- getContents
  let allLines = lines contents
      (rules, updates) = extractRulesAndUpdates allLines
      validUpdates = filter (updateIsValid rules) updates
      midNumbers = map (\l -> read (l !! ((length l-1) `div` 2)) :: Int) validUpdates
      sumOfMidNumbers = sum midNumbers
  -- print $ allLines
  -- print $ rules
  -- print $ updates
  -- print $ midNumbers
  print $ sumOfMidNumbers
--}

extractRulesAndUpdates :: [String] -> (Map String [String], [[String]])
extractRulesAndUpdates ls = (rules, updates)
  where splitIndex = indexOf 0 "" ls
        (rawRules, mt:rawUpdates) = splitAt splitIndex ls  -- first elem in updates results in the empty string ""
        rulesList = map ((\x -> (x!!0, x!!1)) . splitOn "|") rawRules
        rules = createRules (fromList []) rulesList
        updates = map (splitOn ",") rawUpdates
        indexOf i s (x:xs) = if x == s
                             then i
                             else indexOf (i+1) s xs

createRules m []         = m
createRules m ((x,y):rs) = case lookup x m of
                             Nothing -> createRules (insert x [y] m) rs
                             Just r -> createRules (insert x (y:r) m) rs

updateIsValid :: (Map String [String]) -> [String] -> Bool
updateIsValid rules []       = True
updateIsValid rules (_:[])   = True
updateIsValid rules (a:b:us) = case maybeYs of
                                 Nothing -> updateIsValid rules (b:us)
                                 Just ys -> if elem a ys  -- if there is a rule «"b" before "a"»
                                           then False
                                           else updateIsValid rules (b:us)
  where maybeYs = lookup b rules
