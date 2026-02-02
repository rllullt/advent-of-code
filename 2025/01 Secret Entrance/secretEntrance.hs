import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Direction = L | R
  deriving (Show, Eq)

data Instruction = Instruction Direction Int
  deriving (Show, Eq)

main = do
  contents <- getContents
  let dialPos = 50
      instructions = mapMaybe parseRotation $ lines contents  -- mapMaybe is a map, but omits (filters out) Nothing values in the list
      -- rotations = zipWith rotate (dialPos : rotations) instructions
      -- result = foldl (\x y -> if y == 0 then 1 + x else x) 0 rotations
      rotations = zipWith rotate2 ((dialPos, 0) : rotations) instructions
      result = foldl rotate2 (dialPos, 0) instructions
      
  -- print $ instructions
  -- print $ rotations
  print $ result

rotate :: Int -> Instruction -> Int
rotate dialPos (Instruction L count) = (dialPos - count) `mod` 100
rotate dialPos (Instruction R count) = (dialPos + count) `mod` 100

rotate2 :: (Int, Int) -> Instruction -> (Int, Int)
rotate2 (dialPos, count) (Instruction L amount) = (position, passesOver0 + count)
  where
    position = (dialPos - amount) `mod` 100
    passesOver0 = if dialPos == 0
                  then amount `div` 100
                  else if amount == dialPos
                       then 1
                       else if amount > dialPos
                            then 1 + (abs (dialPos - amount) `div` 100)
                            else 0
rotate2 (dialPos, count) (Instruction R amount) = (position, passesOver0 + count)
  where
    position = (dialPos + amount) `mod` 100
    passesOver0 = (dialPos + amount) `div` 100

parseRotation :: String -> Maybe Instruction
parseRotation []     = Nothing
parseRotation (c:cs) =
  case c of
    'L' -> Instruction L <$> readMaybe cs  -- == fmap (Rotation L) (readMaybe cs) :: Maybe Rotation  (Just Rotation o Nothing)
    'R' -> Instruction R <$> readMaybe cs
    _   -> Nothing

