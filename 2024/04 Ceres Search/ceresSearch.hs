import Text.Regex.TDFA

main = do
  contents <- getContents
  -- Generar una lista con:
  --   Todas las listas horizontales
  --   Todas las listas verticales
  --   Todas las listas diagnoales, de arriba e izq a abajo y der y de arrba y der a abajo e izq
  --   Contar todas las apariciones de XMAS en cada una de las listas, desde atrás hacia adelante y desde adelante hacia atrás
  -- Ej: ....XXMAS.
  --     .SAMXMS...
  --     ...S..A...
  --     ..A.A.MS.X
  --     XMASAMX.MM
  --     X.....XA.A
  --     S.S.S.S.SS
  --     .A.A.A.A.A
  --     ..M.M.M.MM
  --     .X.X.XMASX
  -- Entrega 18
  let horLines = lines contents
      reversedHorLines = map reverse horLines
      n = length $ horLines !! 0
      horLinesWDots = (map (\i -> hLs !! i ++ replicate (2*n) '.') [0..2*n-1])
        where hLs = horLines ++ [replicate (2*n) '.' | i <- [0..n-1]]
      reversedHorLinesWDots = (map (\i -> hLs !! i ++ replicate (2*n) '.') [0..2*n-1])
        where hLs = reversedHorLines ++ [replicate (2*n) '.' | i <- [0..n-1]]
      verLines = extractVerLines 0 horLines
      diagLinesIzqDer = extractDiagLines n 3 0 (+1) horLinesWDots
      diagLinesDerIzq = extractDiagLines n 3 0 (+1) reversedHorLinesWDots
  -- print $ horLinesWDots
  -- print $ verLines
  -- print $ diagLinesIzqDer
  -- print $ diagLinesDerIzq
  let xmasCount = sum $ map countXmas [horLinesWDots, verLines, diagLinesIzqDer, diagLinesDerIzq]
  print $ xmasCount

extractVerLines :: Int -> [String] -> [String]
extractVerLines k [] = []
extractVerLines k hLs
  | (length hLs) == k = []
  | otherwise         = (map (\l -> l !! k) hLs) : extractVerLines (k+1) hLs

extractDiagLines :: Int -> Int -> Int -> (Int -> Int) -> [[Char]] -> [[Char]]
extractDiagLines n i j f_j [] = []
extractDiagLines n i j f_j ls
  | i == 2*n-4 = []
  | otherwise = extractDiagLine i j f_j ls : extractDiagLines n (i+1) j f_j ls

extractDiagLine :: Int -> Int -> (Int -> Int) -> [[Char]] -> [Char]
extractDiagLine i j f_j [] = []
extractDiagLine i j f_j ls
  | i == -1 = []
  | otherwise = ls !! i !! j : extractDiagLine (i-1) (f_j j) f_j ls

countXmas :: [String] -> Int
countXmas = sum . map (\l -> textMatches l + textMatches (reverse l))
    where pat = "XMAS" -- RegExp
          textMatches :: String -> Int
          textMatches l = length $ getAllTextMatches (l =~ pat :: AllTextMatches [] String)
