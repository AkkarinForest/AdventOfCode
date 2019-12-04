module Lib
    ( passwordsCount
    ) where

passwordsCount :: IO ()
passwordsCount = print $ allAllComb - norepeat

allAllComb = allComb 6 4 + (allComb 4 9 - allComb 4 4)

allComb digitNumber till = pink (digitNumber - 1) 9 till

pink 1 x _         = sum [1..x]
pink counter x end = sum $ [ pink (counter - 1) n 9 | n <- [(10-end)..x] ]

norepeat = pink 5 4 9

