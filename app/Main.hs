module Main where

import AoCLib
import System.Environment (getArgs)

pickDay :: String -> (String -> IO ())
pickDay day
  | day == "1" = day01
  | otherwise = \_ -> print "Specified day is unimplemented"

main :: IO ()
main = do
  filename_list <- getArgs
  let day = head filename_list
  let filename = last filename_list
  pickDay day filename
