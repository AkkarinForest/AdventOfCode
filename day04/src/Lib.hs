module Lib
    ( passwordsCount
    ) where

passwordsCount :: IO ()
passwordsCount = print $ allCombinations - norepeat

allCombinations = combinationsCount (6 - 1) 9 4 + combinationsCount (4 - 1) 5 9

norepeat = combinationsCount (6 - 1) 4 9

combinationsCount 1 start _         = sum [1..start]
combinationsCount counter start end = sum $ [ combinationsCount (counter - 1) n 9 | n <- [(10-end)..start] ]

-- input = "10847-562041" -> "111111-559999" -> "111111-4999999" and "555555-559999"
