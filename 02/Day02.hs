module Day02 where

solve :: IO ()
solve = do 
  raw <- readFile "02/input.txt"
  let strategy = words <$> lines raw
  print $ sum $ calculateScore <$> strategy
  print $ sum $ calculateScore2 <$> strategy

calculateScore :: [String] -> Int
calculateScore s 
    | s == ["A", "X"] = 3 + 1
    | s == ["A", "Y"] = 6 + 2
    | s == ["A", "Z"] = 0 + 3
    | s == ["B", "X"] = 0 + 1
    | s == ["B", "Y"] = 3 + 2
    | s == ["B", "Z"] = 6 + 3
    | s == ["C", "X"] = 6 + 1
    | s == ["C", "Y"] = 0 + 2
    | s == ["C", "Z"] = 3 + 3
    | otherwise = -999999

calculateScore2 :: [String] -> Int
calculateScore2 s 
    | s == ["A", "X"] = 0 + 3
    | s == ["A", "Y"] = 3 + 1
    | s == ["A", "Z"] = 6 + 2
    | s == ["B", "X"] = 0 + 1
    | s == ["B", "Y"] = 3 + 2
    | s == ["B", "Z"] = 6 + 3
    | s == ["C", "X"] = 0 + 2
    | s == ["C", "Y"] = 3 + 3
    | s == ["C", "Z"] = 6 + 1
    | otherwise = -999999
