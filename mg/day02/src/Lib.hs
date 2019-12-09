module Lib
    ( main
    ) where

import qualified Data.Map.Strict as M

main = case filter (\(_,_, t) -> (run 0 t >>= M.lookup 0)==Just sought) trials of
  ((noun, verb, _):_) -> print $ 100 * noun + verb
  _                   -> print "no result"

initial :: Int -> Int -> M.Map Int Int
initial noun verb = M.insert 2 verb $ M.insert 1 noun $ M.fromList $ zip [0..] input

trials = do
  noun <- [0..99]
  verb <-[0..99]
  pure $ (noun, verb, initial noun verb)

getOp :: Int -> Maybe (Int -> Int -> Int)
getOp 1 = Just (+)
getOp 2 = Just (*)
getOp _ = Nothing

run :: Int -> M.Map Int Int -> Maybe (M.Map Int Int)
run ip acc = do
  opCode <- M.lookup ip acc
  case opCode of
    99 -> pure acc
    n -> do
      aPos <- M.lookup (ip + 1) acc
      aVal <- M.lookup aPos acc
      bPos <- M.lookup (ip + 2) acc
      bVal <- M.lookup bPos acc
      cPos <- M.lookup (ip + 3) acc
      op <- getOp n
      run (ip + 4) $ M.insert cPos (op aVal bVal) acc


sought =19690720

input :: [Int]
input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,13,19,23,2,23,9,27,1,6,27,31,2,10,31,35,1,6,35,39,2,9,39,43,1,5,43,47,2,47,13,51,2,51,10,55,1,55,5,59,1,59,9,63,1,63,9,67,2,6,67,71,1,5,71,75,1,75,6,79,1,6,79,83,1,83,9,87,2,87,10,91,2,91,10,95,1,95,5,99,1,99,13,103,2,103,9,107,1,6,107,111,1,111,5,115,1,115,2,119,1,5,119,0,99,2,0,14,0]
