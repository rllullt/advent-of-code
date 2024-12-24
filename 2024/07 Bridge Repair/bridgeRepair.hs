import Data.List.Split
import Data.Maybe (isJust, fromMaybe)

{-- First part
--}
main = do
  contents <- getContents
  let eqs = map getEq $ lines contents
      solvEqs = map (\eq -> resolverEq (fst eq) (snd eq)) eqs
      justSolvEqs = filter isJust solvEqs
      nums = map (fst . fromMaybe (0, [])) justSolvEqs
      theSum = sum nums
  -- print "Hola"
  -- print eqs
  -- print solvEqs
  -- print justSolvEqs
  -- print nums
  print theSum
--}

resolverEq :: Int -> [Int] -> Maybe (Int, [String])
resolverEq n (x:[]) = if n == x then Just (0, []) else Nothing
resolverEq n (x:xs) =
  let res = let res_sum = resolverEq (n - last xs) (take (length xs) (x:xs))
            in case res_sum of
                 Just (i, l) -> Just (n, l ++ ["+"])
                 Nothing -> let divisible = n `mod` last xs == 0
                                res_mult = if divisible
                                           then resolverEq (n `div` last xs) (take (length xs) (x:xs))
                                           else Nothing
                            in case res_mult of
                                 Just (i, l) -> Just (n, l ++ ["*"])
                                 Nothing -> Nothing
  in res

getEq :: String -> (Int, [Int])
getEq s = (val, nums)
  where splitted = splitOn ":" s
        val = read . head $ splitted :: Int
        nums = map (\x -> read x :: Int) . words $ last splitted