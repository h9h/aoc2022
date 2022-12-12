{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import Algorithm.Search (dijkstra)
import Data.Char (ord)
import Data.List (elemIndices, intercalate)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))

type Position = (Int, Int)
type Terrain = [String]
type Cost  = Int

output :: Maybe (Int, [(Int, Int)]) -> String
output x = case x of
  Just (n, _) -> show n
  Nothing     -> error "fail"

solve :: IO ()
solve = do
  raw <- readFile "12/input.txt"
  let terrain = lines raw
  print $ getStart terrain
  print $ getValue terrain $ getStart terrain
  print $ neighbors terrain $ getStart terrain
  print $ "Part 1           : " ++ output (solveShortestPath terrain (getStart terrain))
  print $ "Part 2           : " ++ show (findShortestHike terrain)

getPos :: Char -> Terrain -> Position
getPos char' t = (r,c)
  where r = fst . head . filter (\(_, s) -> char' `elem` s) $ zip [0..] t
        c = fst . head . filter (\(_, s) -> char' == s) $ zip [0..] (t !! r)

getStart :: Terrain -> Position
getStart = getPos 'S'

getValue :: Terrain -> Position -> Char
getValue t (r,c) 
  | r < 0 || r > length t - 1         = '.'
  | c < 0 || c > length (head t) - 1  = '.' 
  | otherwise                         = t !! r !! c

neighbors :: Terrain -> Position -> [Position]
neighbors t (x, y) = filter (\p -> let v = getValue t p in v /= '.' && accessible v v') [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
  where v' = getValue t (x, y)

accessible :: Char -> Char -> Bool
accessible v   'S' = accessible v  'a'
accessible 'E' v'  = accessible 'z' v'
accessible v   v'  = ord v - ord v' <= 1

{-
>>> accessible 'b' 'a'
True
-}

cost :: Terrain -> Position -> Position -> Cost
cost _ _ _ = 1

finished :: Terrain -> Position -> Bool
finished t (r,c) = 'E' == t !! r !! c

solveShortestPath :: Terrain -> Position -> Maybe (Cost, [Position])
solveShortestPath t = dijkstra (neighbors t) (cost t) (finished t)

{-
>>> solveShortestPath (lines "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi") (0,0)
Just (31,[(0,1),(1,1),(2,1),(2,2),(3,2),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(3,7),(2,7),(1,7),(0,7),(0,6),(0,5),(0,4),(0,3),(1,3),(2,3),(3,3),(3,4),(3,5),(3,6),(2,6),(1,6),(1,5),(1,4),(2,4),(2,5)])
-}

findAllStarts :: Terrain -> [Position]
findAllStarts t = findPositions . map (\c -> if c == 'S' then 'a' else c) $ intercalate "" t
  where findPositions s = map (\p -> (p `div` n, p `mod` n)) $ elemIndices 'a' s
        n = length $ head t

{-
>>> findAllStarts (lines "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")
[(0,0),(0,1),(1,0),(2,0),(3,0),(4,0)]

>>> elemIndices 'a' "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
[1,9,18,27,36]
-}

findShortestHike :: Terrain -> Cost
findShortestHike =  
      fst 
        . minimum 
        . map (fromMaybe (maxBound, []))
        . uncurry map
        . (solveShortestPath &&& findAllStarts)

{-
>>> findShortestHike (lines "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")
29
-}
