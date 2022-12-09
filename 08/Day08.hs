module Day08 where

import Data.Matrix ( getCol, getRow, fromLists, Matrix, ncols, nrows, mapPos, toList )
import Data.Char (digitToInt)
import qualified Data.Vector as V (Vector, slice, toList)

solve :: IO ()
solve = do
  raw <- readFile "08/input.txt"
  let m = getGrid $ lines raw
  print $ countVisibleTrees m
  print $ bestScenicTree m

-- Parse input

lineToInts :: String -> [Int]
lineToInts = map digitToInt

getGrid :: [String] -> Matrix Int
getGrid xs = fromLists $ map lineToInts xs

-- Part 1

countVisibleTrees :: Matrix Int -> Int
countVisibleTrees = length . filter id . toList . visibleTrees

visibleTrees :: Matrix Int -> Matrix Bool
visibleTrees m = mapPos (isVisibleTree m) m

isVisibleTree :: Matrix Int -> (Int, Int) -> Int -> Bool
isVisibleTree m (r, c) e
  | r == 1 || nrows m == r = True -- Matrix is 1-based, Vector 0-base (!?!)
  | c == 1 || ncols m == c = True
  | otherwise = inLineOfSight e (c-1) (getRow r m) || inLineOfSight e (r-1) (getCol c m)

inLineOfSight :: Int -> Int -> V.Vector Int -> Bool
inLineOfSight e i v = all (< e) (V.slice 0 i v) || all (< e) (V.slice (i+1) (n-i-1) v)
  where n = length v

-- Part 2

bestScenicTree :: Matrix Int -> Int
bestScenicTree m = maximum (scenicTrees m)

scenicTrees :: Matrix Int -> Matrix Int
scenicTrees m = mapPos (scenicScoreTree m) m

scenicScoreTree :: Matrix Int -> (Int, Int) -> Int -> Int
scenicScoreTree m (r, c) e
  | r == 1 || nrows m == r = 0 -- Matrix is 1-based, Vector 0-based (!?!)
  | c == 1 || ncols m == c = 0
  | otherwise = scoreInLineOfSight e (c-1) (getRow r m) * scoreInLineOfSight e (r-1) (getCol c m)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if not (p x) then takeUntil p xs else []

scoreInLineOfSight :: Int -> Int -> V.Vector Int -> Int
scoreInLineOfSight e i v = length front * length back
  where front = takeUntil (>= e) (reverse . V.toList $ V.slice 0 i v)
        back  = takeUntil (>= e) (V.toList $ V.slice (i+1) (n-i-1) v)
        n     = length v

-- Tests

{-
>>> countVisibleTrees $ getGrid ["30373", "25512", "65332", "33549", "35390"]
21

>>> scenicScoreTree (getGrid ["30373", "25512", "65332", "33549", "35390"]) (4,3) 5
8
-}
