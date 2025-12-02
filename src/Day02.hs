module Day02 (day02) where

import Data.List (foldl')

splitOn :: Char -> String -> [String]
splitOn x cs = case dropWhile (x ==) cs of
  "" -> []
  cs' -> c : splitOn x cs''
    where
      (c, cs'') = break (x ==) cs'

chunk :: [a] -> Int -> [[a]]
chunk [] _ = []
chunk as n =
  let (a, as') = splitAt n as
   in a : chunk as' n

dup :: [String] -> Bool
dup ss = foldl' (\acc cs -> cs == x && acc) True ss
  where
    x = head ss

invalid :: Int -> Int
invalid n =
  let cs = show n
      len = length cs
      duplicate = dup $ chunk cs $ len `div` 2
   in if even (length cs) && duplicate then n else 0

invalid' :: Int -> Int
invalid' n =
  let cs = show n
      len = length cs `div` 2
      chunks = take len $ map (chunk cs) [1 ..]
      f ss acc = dup ss || acc
      b = foldr f False chunks
   in if b then n else 0

ranges :: String -> [Int]
ranges cs =
  let a = read $ takeWhile (/= '-') cs
      b = read $ tail $ dropWhile (/= '-') cs
   in [a .. b]

day02 :: String -> IO ()
day02 filename = do
  contents <- readFile filename
  putStrLn "\nPart 1:"
  let nums = concatMap ranges $ splitOn ',' $ init contents
  print $ sum $ map invalid nums
  putStrLn "\nPart 2:"
  print $ sum $ map invalid' nums
