module Lib
    ( run
    ) where

import           Data.List.Split

run = do
    input <- readFile "src/input.txt"
    mapM putStrLn . chunksOf width . map toPic . process . chunksOf vol . filter ((/=) '\n') $ input

process (firstL:restL) = foldl (zipWith redraw) firstL restL

redraw t b
  | t == '2'  = b
  | otherwise = t

vol = height * width
height = 6
width = 25

toPic '1' = '*'
toPic _   = ' '
