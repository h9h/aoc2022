module Day09 where

import Text.ParserCombinators.ReadP (ReadP, char, eof, (<++), munch1, choice, readP_to_S, many1)
import Data.Char (isDigit)
import Control.Monad (void)
import Data.List (group, sort)
import GHC.List (foldl')

solve :: IO ()
solve = do
  raw <- readFile "09/input.txt"
  let (path, _, headPos) = walkCommands 1 $ parse raw
  print headPos
  print $ countTouched path
  let (path2, _, headPos2) = walkCommands 9 $ parse raw
  print headPos2
  print $ countTouched path2

data Direction = Up' | Down' | Right' | Left' deriving (Show)
data Command = Command Direction Int deriving (Show)

type Position = (Int, Int)

-- Parsen

pEOL :: ReadP ()
pEOL = void (char '\n') <++ eof

pAmount :: ReadP Int
pAmount = read <$> munch1 isDigit

pUp :: ReadP Direction
pUp = Up' <$ char 'U'

pDown :: ReadP Direction
pDown = Down' <$ char 'D'

pLeft :: ReadP Direction
pLeft = Left' <$ char 'L'

pRight :: ReadP Direction
pRight = Right' <$ char 'R'

pDirection :: ReadP Direction
pDirection = choice [pUp, pDown, pLeft, pRight]

pCommand :: ReadP Command
pCommand = Command <$> pDirection <*> (char ' ' *> pAmount) <* pEOL

parse :: String -> [Command]
parse = fst . head . filter (null . snd) . readP_to_S pCommands
  where
    pCommands :: ReadP [Command]
    pCommands = many1 pCommand

{-
>>> parse "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2" 
[Command Right' 4,Command Up' 4,Command Left' 3,Command Down' 1,Command Right' 4,Command Down' 1,Command Left' 5,Command Right' 2]
-}

-- Movements

walkCommands :: Int -> [Command] -> ([Position], [Position], Position)
walkCommands n = foldl' move ([(0,0)], replicate n (0,0), (0,0))

move :: ([Position], [Position], Position) -> Command -> ([Position], [Position], Position)
move state (Command c n) = last . take (n+1) . iterate (moveStep c) $ state

{-
>>> move ([(0,0)], [(0,0)], (0,0)) (Command Right' 4)
([(0,4),(0,4),(0,4),(0,4),(0,4),(0,3),(0,3),(0,3),(0,3),(0,2),(0,2),(0,2),(0,1),(0,1),(0,0)],[(0,4),(0,4),(0,4),(0,4),(0,4)],(0,4))
-}

moveStep :: Direction -> ([Position], [Position], Position) -> ([Position], [Position], Position)
moveStep d (visited, snake, h) = (newVisited, newSnake, newHead)
  where newHead = adjustHead d h
        newSnake = reverse $ tail $ scanl adjustTail newHead $ reverse snake        
        newVisited = head newSnake : visited-- (if length newSnake > 1 then head (tail newSnake) else head newSnake) : visited

{-
>>> reverse $ tail $ scanl adjustTail (0,4) $ reverse [(0,0), (0,0), (0,2)]
[(0,0),(0,1),(0,3)]

>>> moveStep Right' ([(0,0)], [(0,0), (0,0), (0,2)], (0,4))
([(0,0),(0,0)],[(0,0),(0,1),(0,3)],(0,5))
-}

adjustHead :: Direction -> Position -> Position
adjustHead Up'    (hr, hc) = (hr + 1, hc)
adjustHead Down'  (hr, hc) = (hr - 1, hc)
adjustHead Left'  (hr, hc) = (hr, hc - 1)
adjustHead Right' (hr, hc) = (hr, hc + 1)

adjustTail:: Position -> Position -> Position
adjustTail (hr, hc) (tr, tc) = if abs dr <= 1 && abs dc <= 1 then (tr, tc) else (tr + signum dr, tc + signum dc)
  where (dr, dc) = (hr-tr, hc-tc)

{-
>>> adjustTail (-2, 0) (0, 0)
(-1,0)

>>> adjustTail (-2, -1) (0, 0)
(-1,0)
-}

-- Part 1

countTouched :: [Position] -> Int
countTouched = length . group . sort

{-
>>> countTouched [(2,1),(2,2),(2,3),(3,4),(3,3),(4,2),(4,3),(3,4),(2,4),(1,4),(0,3),(0,2),(0,1),(0,0)]
13
-}
