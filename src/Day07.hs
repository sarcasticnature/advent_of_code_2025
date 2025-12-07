module Day07 (day07) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (elemIndex, foldl')
import Data.Maybe (fromJust)

step :: Int -> (IntSet, Int) -> String -> (IntSet, Int)
step w (is, acc) cs = foldl' f (is, acc) $ zip [0 ..] cs
  where
    safeInsert i is' = if i >= 0 && i < w then IS.insert i is' else is'
    splitBeam i is' = IS.delete i $ safeInsert (i + 1) $ safeInsert (i - 1) is'
    f (is', cnt) (i, c) =
      if c == '^' && IS.member i is
        then (splitBeam i is', cnt + 1)
        else (is', cnt)

countSplits :: String -> Int
countSplits cs =
  let ls = lines cs
      width = length $ head ls
      start = fromJust $ elemIndex 'S' $ head ls
   in snd $ foldl' (step width) (IS.singleton start, 0) $ tail ls

step' :: Int -> IntMap Int -> String -> IntMap Int
step' w im cs = foldl' f im $ zip [0 ..] cs
  where
    safeInsert i n im' = if i >= 0 && i < w then IM.insertWith (+) i n im' else im'
    splitBeam i im' = IM.delete i $ safeInsert (i + 1) val $ safeInsert (i - 1) val im'
      where
        val = im IM.! i
    f im' (i, c) = if c == '^' && IM.member i im then splitBeam i im' else im'

countTimelines :: String -> Int
countTimelines cs =
  let ls = lines cs
      width = length $ head ls
      start = fromJust $ elemIndex 'S' $ head ls
   in sum $ foldl' (step' width) (IM.singleton start 1) $ tail ls

day07 :: String -> IO ()
day07 filename = do
  contents <- readFile filename
  putStrLn "\nPart 1:"
  print $ countSplits contents
  putStrLn "\nPart 2:"
  print $ countTimelines contents
