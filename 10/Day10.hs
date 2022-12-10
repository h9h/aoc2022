module Day10 where

import qualified Data.Vector as V (Vector, snoc, last, length, (!), singleton)
import Data.List.Split (chunksOf)
import Data.List (intercalate)

data Instruction = Noop | AddX Int | Unknown String deriving (Show) 

solve :: IO ()
solve = do
  raw <- readFile "10/input.txt"
  let v = foldl execute (V.singleton 1) (parse raw)
  print $ signalStrength v
  putStrLn $ printScreen $ drawScreen v

parse :: String -> [Instruction]
parse s = parseLine . words <$> lines s
  where
    parseLine ["noop"] = Noop
    parseLine ["addx", n] = AddX (read n)
    parseLine t = Unknown (unwords t)

execute :: V.Vector Int -> Instruction -> V.Vector Int
execute v Noop = V.snoc v (V.last v)
execute v (AddX n) = let l = V.last v in V.snoc (V.snoc v l) (l+n) 
execute _ (Unknown s) = error $ "Unknown instruction: " ++ s

-- Part 1

signalStrength :: V.Vector Int -> Int
signalStrength v = sum [s | i <- [20, 60 .. 220], i <= V.length v, let s = (v V.! (i-1)) * i]

-- Part 2

drawScreen :: V.Vector Int -> [Char]
drawScreen v = map pixel [0 :: Int .. 239]
  where pixel index = if abs((index `mod` 40) - v V.! index) < 2 then '█' else ' '

printScreen :: [Char] -> String
printScreen = intercalate "\n" . chunksOf 40

{-
Mit readInstruction Noop -> [0], AddX n -> [0, n] wäre es noch einfacher:

  sum
    . flip map [20, 60 .. 220]
    . signalStrength
    . scanl (+) 1
    . concatMap readInstruction
    . lines

und

 unlines
    . chunksOf 40
    . zipWith pixel (cycle [0 .. 39])
    . scanl (+) 1
    . concatMap readInstruction
    . lines
-}