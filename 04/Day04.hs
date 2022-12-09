{-# LANGUAGE TypeApplications #-}

module Day04 where
import Data.List.Split (splitOn)


solve :: IO ()
solve = do
  raw <- lines <$> readFile "04/input.txt"
  let pairs = parse <$> raw
  print $ length $ filter fullyContained pairs
  print $ length $ filter overlap pairs


parse :: [Char] -> [[Int]]
parse s = map (map (read @Int) . splitMinus) (splitKomma s)
  where splitKomma = splitOn ","
        splitMinus = splitOn "-"

fullyContained :: Ord a => [[a]] -> Bool
fullyContained [[a1, a2], [b1, b2]] = (a1 <= b1 && a2 >= b2) || (b1 <= a1 && b2 >= a2)
fullyContained _ = False

overlap :: Ord a => [[a]] -> Bool
overlap [[a1, a2], [b1, b2]] = (a2 >= b1) && (b2 >= a1)
overlap _ = False
