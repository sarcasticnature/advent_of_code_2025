module Day06 (day06) where

import Data.List (transpose, zipWith4, zipWith5)

-- Part 1

fanout4 :: (a -> a -> a -> a -> b) -> [[a]] -> [b]
fanout4 f as = zipWith4 f a b c d
  where
    a = head as
    b = as !! 1
    c = as !! 2
    d = as !! 3

calculate4 :: String -> String -> String -> String -> Int
calculate4 a b c d
  | d == "+" = x + y + z
  | d == "*" = x * y * z
  | otherwise = error $ "Bad operator: " ++ d
  where
    x = read a
    y = read b
    z = read c

fanout5 :: (a -> a -> a -> a -> a -> b) -> [[a]] -> [b]
fanout5 f as = zipWith5 f a b c d e
  where
    a = head as
    b = as !! 1
    c = as !! 2
    d = as !! 3
    e = as !! 4

calculate5 :: String -> String -> String -> String -> String -> Int
calculate5 a b c d e
  | e == "+" = x + y + z + w
  | e == "*" = x * y * z * w
  | otherwise = error $ "Bad operator: " ++ e
  where
    x = read a
    y = read b
    z = read c
    w = read d

-- Part 2

splitBlank :: [String] -> [[String]]
splitBlank cs = case dropWhile (null . words) cs of
  [] -> []
  cs' -> c : splitBlank cs''
    where
      (c, cs'') = break (null . words) cs'

parseHW :: String -> [Int]
parseHW s =
  let ls = lines s
      (ls', op) = splitAt (length ls - 1) ls
      nums = transpose ls'
      ops = words $ head op
      math c ns
        | c == "+" = sum ns
        | c == "*" = product ns
        | otherwise = error $ "Bad operator: " ++ c
   in zipWith math ops $ map (map read) $ splitBlank nums

day06 :: String -> IO ()
day06 filename = do
  contents <- readFile filename
  putStrLn "\nPart 1:"
  -- print $ sum $ fanout4 calculate4 $ map words $ lines contents
  -- print $ sum $ fanout5 calculate5 $ map words $ lines contents
  putStrLn "\nPart 2:"
  print $ sum $ parseHW contents
