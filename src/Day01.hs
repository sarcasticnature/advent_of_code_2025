module Day01 (day01) where

import Data.List (foldl')

parse :: String -> Int
parse cs =
  let c = head cs
      n = read $ tail cs
   in if c == 'R' then n else -n

countZeros :: [Int] -> Int
countZeros ns = fst $ foldl' f (0, 50) ns
  where
    f (cnt, acc) n =
      let n' = acc + n
          n'' = if n' < 0 then head $ dropWhile (< 0) $ iterate (+ 100) n' else n' `mod` 100
          cnt' = if n'' == 0 then cnt + 1 else cnt
       in (cnt', n'')

countMethod :: [Int] -> Int
countMethod ns = fst $ foldl' f (0, 50) ns
  where
    f (cnt, acc) n =
      let n' = acc + n
          n'' = if n' < 0 then head $ dropWhile (< 0) $ iterate (+ 100) n' else n' `mod` 100
          points
            | n' < 0 = length (takeWhile (<= 0) $ iterate (+ 100) n') - fromEnum (acc == 0)
            | n' > 0 = n' `div` 100
            | acc == 0 = 0
            | otherwise = 1
       in (cnt + points, n'')

day01 :: String -> IO ()
day01 filename = do
  contents <- readFile filename
  putStrLn "\nPart 1:"
  print $ countZeros $ map parse $ lines contents
  putStrLn "\nPart 2:"
  print $ countMethod $ map parse $ lines contents
