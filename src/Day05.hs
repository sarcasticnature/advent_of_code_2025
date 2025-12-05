module Day05 (day05) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

parseRanges :: [String] -> [(Int, Int)]
parseRanges ss = map parse ss'
  where
    ss' = takeWhile (not . null) ss
    parse cs =
      let low = read $ takeWhile (/= '-') cs
          high = read $ tail $ dropWhile (/= '-') cs
       in (low, high)

parseIngredients :: [String] -> [Int]
parseIngredients ss = map read $ tail $ dropWhile (not . null) ss

fresh :: [(Int, Int)] -> [Int] -> Int
fresh rs ns = length $ filter id $ map f ns
  where
    f n = foldr (\(low, high) b -> b || n >= low && n <= high) False rs

insertRange :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
insertRange rs (low, high) =
  let f (b, acc) (l, h)
        | low < l && high >= l = (True, Set.insert (low, max high h) acc)
        | high > h && low <= h = (True, Set.insert (min low l, high) acc)
        | low >= l && high <= h = (True, Set.insert (l, h) acc)
        | otherwise = (b, Set.insert (l, h) acc)
      (found, rs') = Set.foldl' f (False, Set.empty) rs
   in if found then rs' else Set.insert (low, high) rs

dedupe :: Set (Int, Int) -> Set (Int, Int)
dedupe = foldl' insertRange Set.empty

count :: Set (Int, Int) -> Integer
count = Set.foldl' (\acc (low, high) -> acc + fromIntegral (high - low) + 1) 0

day05 :: String -> IO ()
day05 filename = do
  rows <- lines <$> readFile filename
  putStrLn "\nPart 1:"
  let ranges = parseRanges rows
  let ingredients = parseIngredients rows
  print $ fresh ranges ingredients
  putStrLn "\nPart 2:"
  print $ count $ dedupe $ Set.fromList ranges
