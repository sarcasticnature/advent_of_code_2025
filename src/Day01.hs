module Day01 (day01) where

day01 :: String -> IO ()
day01 filename = do
  contents <- readFile filename
  putStrLn "\nPart 1:"
  print contents
  putStrLn "\nPart 2:"
