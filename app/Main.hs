module Main where

import AoCLib
import System.Environment (getArgs)

pickDay :: String -> (String -> IO ())
pickDay day
  | day == "1" = day01
  | day == "2" = day02
  | day == "3" = day03
  | day == "4" = day04
  | day == "5" = day05
  | day == "6" = day06
  | otherwise = \_ -> print "Specified day is unimplemented"

main :: IO ()
main = do
  filename_list <- getArgs
  let day = head filename_list
  let filename = last filename_list
  pickDay day filename
