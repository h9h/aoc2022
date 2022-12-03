module Day03 where
  
import Data.List (group, sort, nub)
import Data.List.Split (chunksOf)

solve :: IO ()
solve = do 
  raw <- readFile "03/input.txt"
  let rucksacks = splitHalf <$> lines raw
  print $ sum $ typeOfItem . head . intersect <$> rucksacks
  let groups = chunksOf 3 $ lines raw
  print $ sum $ typeOfItem . (intersect3 $$$) <$> groups

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

typeOfItem :: Char -> Int
typeOfItem s
    | fromEnum s >= 97 = fromEnum s - 97 + 1
    | otherwise = fromEnum s - 65 + 27

intersect :: Eq a => ([a], [a]) -> [a]
intersect ([], _) = []
intersect (xs, ys) = filter (`elem` xs) ys

($$$) :: (a -> a -> a -> Char) -> [a] -> Char
f $$$ (x:y:z:_) = f x y z
_ $$$ [] = '0'
_ $$$ [_] = '0'
_ $$$ [_, _] = '0'

intersect3 :: String -> String -> String -> Char
intersect3 a b c = head . map head . filter (not . null . drop 2) . group . sort $ nub a ++ nub b ++ nub c
