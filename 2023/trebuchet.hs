-- module Main where

-- import PGF
-- import System.Environment
-- import System.IO

-- readData :: FilePath -> IO (PGF, String)
-- readData file = do
    -- gr <- readPGF file
    -- content <- readFile "trebuchet.txt"
    -- return (gr, content)

-- processData :: PGF -> String -> String
-- processData gr content = undefined

-- outputData :: String -> IO ()
-- outputData contents = writeFile "output.txt" contents

-- main :: IO ()
-- main = do
    -- file:_ <- getArgs
    -- (gr, content) <- readData file
    -- outputData $ processData gr content
    -- putStrLn "bye"
-- -- Or simply
-- -- main = do
-- --     file:_ <- getArgs
-- --     readData file >>= outputData . processData . uncurry
-- --     putStrLn "bye"


-- import Data.Ord

-- import Data.List

-- main :: IO ()

-- main = do

  -- let nums = [1,2]

  -- fl <- getAllLengths nums

  -- putStrLn "fl is, "
  -- print fl



-- filename :: Int -> FilePath
-- filename i = "file" ++ show i ++ ".dat"

-- fileLength :: FilePath -> IO Int
-- fileLength file = fmap length (readFile file)

-- getAllLengths :: [Int] -> IO [Int]
-- getAllLengths nums = mapM (fileLength . filename) nums



-- import System.IO  
-- import Control.Monad

-- main = do  
        -- let list = []
        -- handle <- openFile "trebuchet.txt" ReadMode
        -- contents <- hGetContents handle
        -- let singlewords = words contents
            -- list = f singlewords
        -- print list
        -- hClose handle   

-- f :: [String] -> [Int]
-- f = map read



-- main = do  
  -- contents <- readFile "trebuchet.txt"
  -- print . map readInt . words $ contents
-- -- alternately, main = print . map readInt . words =<< readFile "test.txt"

-- readInt :: String -> Int
-- readInt = read




import Data.Char
import Text.Read


main = do  
  contents <- getContents  -- lista de string
  putStr contents
  -- let numbers = map getNums contents
  -- putStr (map toUpper numbers)


-- getNums :: String -> [Int]
-- getNums [] = []
-- getNums [x:xs] = readMaybe x : getNums xs
