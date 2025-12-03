module Day03 (day03) where

import Data.Char (digitToInt)
import Data.List (foldl')

joltage :: [Int] -> Int
joltage ns =
  let f_l (max', idx) (n, i) = if max' < n then (n, i) else (max', idx)
      (l, start) = foldl' f_l (0, -1) $ zip (init ns) [1 ..]
      r = foldl' max 0 $ drop start ns
   in 10 * l + r

joltage' :: Int -> Int -> [Int] -> Int
joltage' acc 0 _ = acc
joltage' acc i ns =
  let ns' = reverse $ drop (i - 1) $ reverse ns
      f (max', idx) (n, i') = if max' < n then (n, i') else (max', idx)
      (l, start) = foldl' f (0, -1) $ zip ns' [1 ..]
      acc' = 10 * acc + l
   in joltage' acc' (i - 1) $ drop start ns

day03 :: String -> IO ()
day03 filename = do
  contents <- readFile filename
  let ints = map (map digitToInt) $ lines contents
  putStrLn "\nPart 1:"
  print $ sum $ map joltage ints
  putStrLn "\nPart 2:"
  print $ sum $ map (joltage' 0 12) ints
