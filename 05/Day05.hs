{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Data.List.Split (splitOn)
import Data.Text (pack, unpack, replace)
import Data.List (transpose)


solve :: IO ()
solve = do
  raw <- readFile "05/input.txt"
  let parts = splitOn "\n\n" raw
  let stackOfCratesDef = init . lines $ head parts
  let instructionsDef = parts !! 1
  let initialCrates = makeStackOfCrates stackOfCratesDef
  let finalStacks1 = foldl (parseInstruction 'I') initialCrates $ lines instructionsDef
  print $ concatMap head finalStacks1
  let finalStacks2 = foldl (parseInstruction 'B') initialCrates $ lines instructionsDef
  print $ concatMap head finalStacks2


countStacks :: [Char] -> Int
countStacks line = read @Int $ last $ filter (/= "") $ splitOn " " line

parseCargo :: String -> [String]
parseCargo = map (filter (\c -> c /= '[' && c /= ']' && c /= '.')) <$> splitOn " " . unpack . replace "    " " " . pack

{-
>>> parseCargo "            [M] [S] [S]            "
["","","","M","S","S","","",""]
-}

makeStackOfCrates :: [String] -> [[String]]
makeStackOfCrates s = map (filter (/= "" )) <$> transpose $ parseCargo <$> s

{-
>>> makeStackOfCrates ["            [M] [S] [S]            ", "        [M] [N] [L] [T] [Q]        "]
[[],[],["M"],["M","N"],["S","L"],["S","T"],["Q"],[],[]]
-}


parseInstruction :: Char -> [[String]] -> String -> [[String]]
parseInstruction mode stackOfCrates ins = instr $ splitOn " " ins
  where instr  s = case s of
                    ["move", n, "from",  a , "to", b ] -> case mode of
                        'I' -> moveIterative stackOfCrates (read @Int n) (read @Int a) (read @Int b)
                        'B' -> moveBatch stackOfCrates (read @Int n) (read @Int a) (read @Int b)
                        _   -> stackOfCrates
                    _ -> stackOfCrates

moveIterative :: [[String]] -> Int -> Int -> Int -> [[String]]
moveIterative stackOfCrates n a b = case n of
  1 -> replaceStack added (a - 1) $ tail source
          where added  = replaceStack stackOfCrates (b - 1) $ head source : target
                source = stackOfCrates !! (a - 1)
                target = stackOfCrates !! (b - 1)
  0 -> stackOfCrates
  m -> moveIterative (moveIterative stackOfCrates 1 a b) (m-1) a b

{-
>>> moveIterative (makeStackOfCrates ["            [M] [S] [S]            ", "        [M] [N] [L] [T] [Q]        "]) 2 4 3
[[],[],["N","M","M"],[],["S","L"],["S","T"],["Q"],[],[]]
-}

moveBatch :: [[String]] -> Int -> Int -> Int -> [[String]]
moveBatch stackOfCrates n a b = 
  replaceStack added (a - 1) remainder
          where added  = replaceStack stackOfCrates (b - 1) $ moved ++ target
                (moved, remainder) = splitAt n source
                source = stackOfCrates !! (a - 1)
                target = stackOfCrates !! (b - 1)

replaceStack :: [[String]] -> Int -> [String] -> [[String]]
replaceStack xs i e = case splitAt i xs of
   (before, _:after) -> before ++ e: after
   _ -> xs

{-
>>> moveBatch (makeStackOfCrates ["            [M] [S] [S]            ", "        [M] [N] [L] [T] [Q]        "]) 2 4 3
[[],[],["M","M"],["N"],["S","L"],["S","T"],["Q"],[],[]]
-}
