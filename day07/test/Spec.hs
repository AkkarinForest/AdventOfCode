module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Amplify
import           Lib

main :: IO ()
main = do
  defaultMain (testGroup "Our Library Tests"
    --[amplifyForPhaseLoopedSequence
    --,maxForPhasesLoopedTest
    --,maxForPhasesTest
    --,amplifyForPhaseSequence
    [jumpTestPT
    , jumpTestPF
    , jumpTestIF
    , jumpTestIT])

jumpTestPT :: TestTree
jumpTestPT = testCase "Testing jump position mode"
  (assertEqual "Should be 0 when input 0" ([3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9],11,0,4) (result 0 programP))

jumpTestPF :: TestTree
jumpTestPF = testCase "Testing jump position mode"
  (assertEqual "Should be 1 when input other than 0" ([3,12,6,12,15,1,13,14,13,4,13,99,147,1,1,9],11,1,4) (result 147 programP))

jumpTestIT :: TestTree
jumpTestIT = testCase "Testing jump immidiate mode"
  (assertEqual "Should be 0 when input 0" ([3,3,1105,0,9,1101,0,0,12,4,12,99,0],11,0,4) (result 0 programI))

jumpTestIF :: TestTree
jumpTestIF = testCase "Testing jump immidiate mode"
  (assertEqual "Should be 1 when input other than 0" ([3,3,1105,147,9,1101,0,0,12,4,12,99,1],11,1,4) (result 147 programI))

programP = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
programI = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]

-- amplifyForPhaseSequence :: TestTree
-- amplifyForPhaseSequence = testCase "Testing amplify for sequence"
--   (assertEqual "Should be equal" 43210 (resultForPhase testProgram1 testPhase1))
--
-- maxForPhasesTest :: TestTree
-- maxForPhasesTest = testCase "Testing amplify max"
--   (assertEqual "Should be equal" 43210 (maxForPhases testProgram1))
--
-- testProgram1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
-- testPhase1 = [4,3,2,1,0]
--
--
--
-- amplifyForPhaseLoopedSequence :: TestTree
-- amplifyForPhaseLoopedSequence = testCase "Testing amplify for sequence looped"
--   (assertEqual "Should be equal" 139629729 (resultForPhaseLooped testProgram2 testPhase2))
--
-- maxForPhasesLoopedTest :: TestTree
-- maxForPhasesLoopedTest = testCase "Testing amplify max for looped"
--   (assertEqual "Should be equal" 139629729 (maxForPhasesLooped testProgram2))
--
-- testProgram2 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
-- testPhase2 = [9,8,7,6,5]
--
