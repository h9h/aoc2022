{-# LANGUAGE TypeApplications #-}
module Day01 where

import Data.List.Split (splitWhen)
import Data.List (sort)

solve :: IO ()
solve = 
  do  numStrs <- readFile "01/input.txt"
      let calories = fmap (fmap (read @Int)) $ splitWhen null $ lines numStrs
      print $ part1 calories
      print $ part2 calories

part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . fmap sum

{-
>>> part1 [[1,2], [2,3]]
5
-}
