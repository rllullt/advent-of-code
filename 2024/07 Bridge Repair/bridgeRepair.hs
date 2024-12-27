import Data.List.Split
import Data.Maybe (isJust, fromMaybe)

{-- First part
--
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

{-- Second part
--}
main = do
  contents <- getContents
  let eqs = map getEq $ lines contents
      solvEqs = map (\eq -> resolverEq2 (fst eq) (snd eq)) eqs
      justSolvEqs = filter isJust solvEqs
      nums = map (fst . fromMaybe (0, [])) justSolvEqs
      theSum = sum nums
  -- print eqs
  -- print solvEqs
  -- print justSolvEqs
  -- print nums
  print theSum

resolverEq2 :: Int -> [Int] -> Maybe (Int, [String])
resolverEq2 n (x:[]) = if n == x then Just (0, []) else Nothing
resolverEq2 n (x:y:[]) = if x + y == n
                         then Just (n, ["+"])
                         else if x * y == n
                              then Just (n, ["*"])
                              else if concatInts x y == n
                                   then Just (n, ["||"])
                                   else Nothing
resolverEq2 n (x:xs) =
  let res = let res_sum = resolverEq2 (n - last xs) (take (length xs) (x:xs))
            in case res_sum of
                 Just (i, l) -> Just (n, l ++ ["+"])
                 Nothing -> let divisible = n `mod` last xs == 0
                                res_mult = if divisible
                                           then resolverEq2 (n `div` last xs) (take (length xs) (x:xs))
                                           else Nothing
                            in case res_mult of
                                 Just (i, l) -> Just (n, l ++ ["*"])
                                 Nothing -> let a = antepenult (x:xs) + penult (x:xs)
                                                b = antepenult (x:xs) * penult (x:xs)
                                                c = concatInts (antepenult (x:xs)) (penult (x:xs))
                                                res_concats = map (\z -> resolverEq2 n (take (length xs - 3) (x:xs) ++ [concatInts z (last xs)])) [a, b, c]
                                                justs = filter isJust res_concats
                                            in if length justs > 0
                                               then let Just (i, l) = justs !! 0
                                                    in Just (n, l ++ ["||"])
                                               else Nothing
  in res

penult :: [a] -> a
penult xs = xs !! (length xs - 2)

antepenult xs = xs !! (length xs - 3)

concatInts :: Int -> Int -> Int
concatInts a b = read $ show a ++ show b :: Int
