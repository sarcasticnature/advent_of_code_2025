module Day04 (day04) where

import qualified Data.Vector.Unboxed as VU

parse :: String -> VU.Vector Char
parse = VU.fromList . filter (/= '\n')

neighbors :: Int -> Int -> VU.Vector Char -> Int -> Int
neighbors w h vec i =
  let len = w * h
      ls = [i - w - 1, i - 1, i + w - 1]
      rs = [i - w + 1, i + 1, i + w + 1]
      f idx = vec VU.! idx == '@'
      up = [i - w | i - w >= 0, f (i - w)]
      down = [i + w | i + w < len, f (i + w)]
      left = [x | x <- ls, x >= 0, x < len, x `mod` w /= w - 1, f x]
      right = [x | x <- rs, x >= 0, x < len, x `mod` w /= 0, f x]
   in if not $ f i then -1 else length $ up ++ down ++ left ++ right

revert :: Int -> Char
revert n = if n < 4 then '.' else '@'

spaces :: Int -> Int -> VU.Vector Char -> VU.Vector Char
spaces w h vec = VU.map (revert . neighbors w h vec) ivec
  where
    ivec = VU.fromList [0 .. w * h - 1]

run :: Int -> Int -> VU.Vector Char -> VU.Vector Char
run w h vec =
  let vec' = spaces w h vec
   in if vec == vec' then vec else run w h vec'

diff :: VU.Vector Char -> VU.Vector Char -> Int
diff old new = VU.sum $ VU.zipWith f old new
  where
    f o n = if o == '@' && n == '.' then 1 else 0

-- prettyMap :: Int -> VU.Vector Char -> String
-- prettyMap width vec =
--   let addNew i = if i == 0 then "\n" else ""
--       f (acc, i) c = (c : addNew i ++ acc, (i + 1) `mod` width)
--       rev = fst $ VU.foldl' f ("", 0) vec
--    in tail $ reverse rev

day04 :: String -> IO ()
day04 filename = do
  contents <- readFile filename
  let height = length $ lines contents
  let width = length $ head $ lines contents
  putStrLn "\nPart 1:"
  let vec = parse contents
  print $ diff vec $ spaces width height vec
  putStrLn "\nPart 2:"
  let res = run width height vec
  -- putStrLn $ prettyMap width res
  print $ diff vec res
