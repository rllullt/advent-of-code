import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Direction = L | R
  deriving (Show, Eq)

data Instruction = Instruction Direction Int
  deriving (Show, Eq)

{-- First part
--}
main = do
  contents <- getContents
  let dialPos = 50
      instructions = mapMaybe parseRotation $ lines contents  -- mapMaybe is a map, but omits (filters out) Nothing values in the list
      rotations = zipWith rotate (dialPos : rotations) instructions
      result = foldl (\x y -> if y == 0 then 1 + x else x) 0 rotations
      
      -- prediction = predictPath theMap  -- returns the map with the 'X'
      -- distinctPositions = sum $ map (length . filter (=='X')) prediction
--  print $ instructions
--  print $ rotations
  print $ result
--}

rotate :: Int -> Instruction -> Int
rotate dialPos (Instruction L count) = (dialPos - count) `mod` 100
rotate dialPos (Instruction R count) = (dialPos + count) `mod` 100

parseRotation :: String -> Maybe Instruction
parseRotation []     = Nothing
parseRotation (c:cs) =
  case c of
    'L' -> Instruction L <$> readMaybe cs  -- == fmap (Rotation L) (readMaybe cs) :: Maybe Rotation  (Just Rotation o Nothing)
    'R' -> Instruction R <$> readMaybe cs
    _   -> Nothing

