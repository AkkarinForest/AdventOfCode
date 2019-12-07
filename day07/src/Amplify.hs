{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amplify
    ( run
    , resultForPhase
    , maxForPhases
    ) where

import qualified Data.List    as List
import qualified Data.Text    as T
import           Data.Text.IO as TIO
import qualified Lib


run :: IO ()
run = do
        program :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> TIO.readFile "src/input.txt"
        print $ maxForPhases program

resultForPhase :: [Int] -> [Int] -> Int
resultForPhase program phaseSequence = foldl (\acc phase ->  head $ Lib.result [phase, acc] program) 0 phaseSequence

maxForPhases :: [Int] -> Int
maxForPhases program = maximum $ [resultForPhase program phaseSequence |phaseSequence <- allPhaseSequences]

allPhaseSequences :: [[Int]]
allPhaseSequences = List.permutations [4,3,2,1,0]


