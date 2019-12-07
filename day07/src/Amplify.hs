{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Amplify
    ( run
    ) where

import qualified Data.List    as List
import qualified Data.Text    as T
import           Data.Text.IO as TIO
import           Data.Tuple   as Tpl
import qualified Lib

run :: IO ()
run = do
        program :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> TIO.readFile "src/input.txt"
        print $ maxForPhases program

maxForPhases :: [Int] -> Int
maxForPhases program = maximum $ results
    where results = map (\init -> runLoops init) allInits
          allInits = map (\phases -> initializeAmps amps phases) allPhasesPerms
          amps = amplifiers program

runLoops :: [([Int], Int, Int, Int)] -> Int
runLoops amps
    | code == 99 = acc
    | code == 0  = acc
    | otherwise  = runLoops $ Tpl.snd (List.mapAccumL runOneLoop acc amps)
    where (_,_,acc,code) = last amps

runOneLoop :: Int -> ([Int], Int, Int, Int) -> (Int, ([Int], Int, Int, Int))
runOneLoop input (amp, address, _, _) = (output, newAmp)
    where output = getOutput newAmp
          newAmp = Lib.run input address amp

initializeAmps :: [[Int]] -> [Int] -> [([Int], Int, Int, Int)]
initializeAmps amps phases = [(init1, adr1, input1, code1), (init2, adr2, input2, code2), (init3, adr3, input3, code3), (init4, adr4, input4, code4), (init5, adr5, input5, code5)]
    where  (init5, adr5, input5, code5) = Lib.runInit [(phases!!4),input4] 0 (amps!!4)
           (init4, adr4, input4, code4) = Lib.runInit [(phases!!3),input3] 0 (amps!!3)
           (init3, adr3, input3, code3) = Lib.runInit [(phases!!2),input2] 0 (amps!!2)
           (init2, adr2, input2, code2) = Lib.runInit [(phases!!1),input1] 0 (amps!!1)
           (init1, adr1, input1, code1) = Lib.runInit [(phases!!0),0]      0 (amps!!0)

amplifiers :: [Int] -> [[Int]]
amplifiers prg = replicate 5 prg

allPhasesPerms :: [[Int]]
allPhasesPerms = List.permutations [5,6,7,8,9]

getOutput (_,_,x,_) = x
