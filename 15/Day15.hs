{-# LANGUAGE TypeApplications #-}
module Day15 where

import Data.Range (Range, (+=+), joinRanges, difference, fromRanges, Range (SingletonRange))

type Position = (Int, Int)

solve :: IO ()
solve = do
  raw <- readFile "15/input.txt"
  let positions = parse raw
  print $ "Part 1   : " ++ show (part1 2000000 positions)
  print $ "Part 2   : " ++ show (part2 4000000 positions)

test :: IO ()
test = do
  raw <- readFile "15/test.txt"
  let positions = parse raw
  print $ show positions
  print $ show $ take 10 $ rowCovered 10 positions
  print $ "Test Part 1   : " ++ show (part1 10 positions)

parseline :: [Char] -> (Position, Position)
parseline = t . map (read @Int) . words . filter (`elem` " -0123456789")
  where t [s1, s2, b1, b2] = ((s1, s2), (b1, b2))
        t _ = error "Invalid parse"

parse :: String -> [(Position, Position)]
parse = map parseline . lines

manhattan :: Position -> Position -> Int
manhattan (p1,p2) (q1,q2) = abs (p1 - q1) + abs (p2 - q2)

rowCoveredBySensor :: Int -> (Position, Position) -> [Range Int]
rowCoveredBySensor y (s@(x1, y1), b) = [(x1 - delta) +=+ (x1 + delta) | delta >= 0]
  where delta = dist - abs (y1 - y)
        dist  = manhattan s b

rowCovered :: Int -> [(Position, Position)] -> [Range Int]
rowCovered y = joinRanges . concatMap (rowCoveredBySensor y) 

part1 :: Int -> [(Position, Position)] -> Int
part1 row ps = length . fromRanges $ difference (rowCovered row ps) [SingletonRange x | (x, y) <- uncurry (++) $ unzip ps, y == row]

{-
>>> rowCoveredBySensor 0 ((5, 0), (3, 0))
[3 +=+ 7]

>>> rowCoveredBySensor 1 ((5, 0), (3, 0))
[4 +=+ 6]

>>> rowCovered 2 [((5, 0), (3, 0)), ((-1, 0), (-5, 0))]
[-3 +=+ 1,SingletonRange 5]
-}

part2 :: Int -> [(Position, Position)] -> Int
part2 rows ps = 
  let notCovered = [([0 +=+ rows] `difference` rowCovered r ps, r) | r <- [0 .. rows]]
      (px, py) = head $ concatMap (\(range, y) -> zip (fromRanges range) [y, y..]) notCovered
   in px * 4000000 + py
