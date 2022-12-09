module Puzzle where

fibs :: Int -> [Int]
fibs n = 1 : n : next (fibs n)
  where
    next (a : t@(b:_)) = (a+b) : next t

solution :: [[Int]]
solution = take 1 $ [xs | n <- [1..], let fs = fibs n, let xs = takeWhile (<= 1000000) fs, 1000000 `elem` xs]

{-
>>> solution
[[1,999999,1000000]]
-}
