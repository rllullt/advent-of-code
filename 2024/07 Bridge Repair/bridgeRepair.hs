main = do
  print "Hola"

resolver_eq :: Int -> [Int] -> Maybe [String]
resolver_eq n (x:[]) = if n == x then Just [] else Nothing
resolver_eq n (x:xs) =
  let res = let res_sum = resolver_eq (n - last xs) (take (length xs) (x:xs))
            in case res_sum of
                 Just l -> Just (l ++ ["+"])
                 Nothing -> let divisible = n `mod` last xs == 0
                                res_mult = if divisible
                                           then resolver_eq (n / last xs) (take (length xs) (x:xs))
                                           else Nothing
                            in case res_mult of
                                 Just l -> Just (l ++ ["*"])
                                 Nothing -> Nothing
  in res
