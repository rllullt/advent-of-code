import Data.List
import Data.Maybe

{-- First part
--}
main = do
  contents <- getContents
  let theMap = lines contents
      prediction = predictPath theMap  -- returns the map with the 'X'
      distinctPositions = sum $ map (length . filter (=='X')) prediction
  print $ theMap
--}

-- Marks with a 'X' the positions where the guard will pass
predictPath :: [[Char]] -> [[Char]]
predictPath m =
  let rows = m
      (i, j) = getGuardPos rows
      arrow = m !! i !! j
      -- newRows = case arrow of
                  -- '^' -> if i > 0 then rows 
  in m
  where getGuardPos rs =
          let maybePossJ = map (findIndex (\x -> x /= '.' && x /= '#')) rs
              Just (Just j) = find isJust maybePossJ
              Just i = findIndex isJust maybePossJ
          in (i, j)
