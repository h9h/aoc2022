module Day11 where

import Data.List (sort)

solve :: IO ()
solve = do
  -- raw <- readFile "11/input.txt"
  -- print raw
  let inputMonkeys = initializeMonkeys
  let inputItems = initializeItems
  print $ take 21 $ iterate (playRound1 inputMonkeys) (replicate (length inputMonkeys) 0, inputItems)
  let result = last $ take 21 $ iterate (playRound1 inputMonkeys) (replicate (length inputMonkeys) 0, inputItems)
  let counts = reverse $ sort $ fst result
  print counts
  print $ head counts * counts !! 1
  let result2 = last $ take 10001 $ iterate (playRound2 inputMonkeys) (replicate (length inputMonkeys) 0, inputItems)
  let counts2 = reverse $ sort $ fst result2
  print $ head counts2 * counts2 !! 1

type Item = (Int, Integer)
type Items = [Item]
type Operation = Integer -> Integer
type Test = Integer -> Bool
type Targets = (Int, Int)

data Monkey = Monkey {
    key :: Int,
    operation :: Operation,
    predicate :: Test,
    targets :: Targets
  }

type Monkeys = [Monkey]

initializeTestMonkeys :: Monkeys
initializeTestMonkeys = [
    Monkey 0 (* 19)        (\n -> n `mod` 23 == 0) (2,3),
    Monkey 1 (+ 6)         (\n -> n `mod` 19 == 0) (2,0),
    Monkey 2 (\n -> n * n) (\n -> n `mod` 13 == 0) (1,3),
    Monkey 3 (+ 3)         (\n -> n `mod` 17 == 0) (0,1)
  ]

initializeTestItems :: Items
initializeTestItems = [
    (0, 79), (0, 98),
    (1, 54), (1, 65), (1, 75), (1, 74),
    (2, 79), (2, 60), (2, 97),
    (3, 74)
  ]

initializeMonkeys :: Monkeys
initializeMonkeys = [
    Monkey 0 (* 5)         (\n -> n `mod`  7 == 0) (6,7),
    Monkey 1 (+ 1)         (\n -> n `mod` 17 == 0) (0,6),
    Monkey 2 (+ 8)         (\n -> n `mod` 11 == 0) (5,3),
    Monkey 3 (+ 4)         (\n -> n `mod` 13 == 0) (0,1),
    Monkey 4 (+ 7)         (\n -> n `mod` 19 == 0) (5,2),
    Monkey 5 (+ 2)         even                    (1,3),
    Monkey 6 (* 11)        (\n -> n `mod`  5 == 0) (7,4),
    Monkey 7 (\n -> n * n) (\n -> n `mod`  3 == 0) (4,2)
  ]

productDivisors :: Integer
productDivisors = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

initializeItems :: Items
initializeItems = [
    (0,89),(0,84),(0,88),(0,78),(0,70),
    (1,76),(1,62),(1,61),(1,54),(1,69),(1,60),(1,85),
    (2,83),(2,89),(2,53),
    (3,95),(3,94),(3,85),(3,57),
    (4,82),(4,98),
    (5,69),
    (6,82),(6,70),(6,58),(6,87),(6,59),(6,99),(6,92),(6,65),
    (7,91),(7,53),(7,96),(7,98),(7,68),(7,82)
  ]

applyPart1 :: Monkey -> Item -> Item
applyPart1 m item = (newMonkey, value)
  where value = operation m (snd item) `div` 3
        newMonkey = if predicate m value then fst (targets m) else snd (targets m)

applyPart2 :: Monkey -> Item -> Item
applyPart2 m item = (newMonkey, value)
  where value = operation m (snd item) `mod` productDivisors
        newMonkey = if predicate m value then fst (targets m) else snd (targets m)

{-
>>> applyPart1 (initializeTestMonkeys !! 0) (0, 79) 
(3,500)
-}


playMonkey :: (Monkey -> Item -> Item) -> Monkey -> ([Int], Items) -> ([Int], Items)
playMonkey f m (c, items) = (count, new ++ old)
  where k = key m
        new = f m <$> filter (\(j, _) -> j == k) items
        old = filter (\(j, _) -> j /= k) items
        count = map (\i -> c !! i + if i == k then length new else 0) [0..length c - 1]

{-
>>> playMonkey applyPart1 (initializeTestMonkeys !! 0) ([0,5,0,0], initializeTestItems)
([8,5,0,0],[(3,500),(3,620),(1,54),(1,65),(1,75),(1,74),(2,79),(2,60),(2,97),(3,74)])
-}

seq' :: [a -> a] -> a -> [a]
seq' fs z = scanl (\x f -> f x) z fs

playRound1 :: Monkeys -> ([Int], Items) -> ([Int], Items)
playRound1 ms = last . seq' (playMonkey applyPart1 <$> ms)

playRound2 :: Monkeys -> ([Int], Items) -> ([Int], Items)
playRound2 ms = last . seq' (playMonkey applyPart2 <$> ms)

{-
>>> playRound1 initializeTestMonkeys ([0,0,0,0], initializeTestItems)
([2,4,3,5],[(1,401),(1,1046),(1,167),(1,207),(1,25),(1,2080),(0,20),(0,23),(0,27),(0,26)])

>>> last $ take 21 $ iterate (playRound1 initializeTestMonkeys) ([0,0,0,0], initializeTestItems)
([101,95,7,105],[(1,115),(0,34),(1,199),(1,53),(1,93),(1,245),(0,26),(0,14),(0,12),(0,10)])

>>> last $ take 10001 $ iterate (playRound2 initializeTestMonkeys) ([0,0,0,0], initializeTestItems)
([50695,49301,661,50640],[(1,9201133),(0,949756),(1,4050898),(1,312078),(1,690729),(1,1166945),(1,6216613),(0,667213),(0,4587084),(0,1153594)])
-}
