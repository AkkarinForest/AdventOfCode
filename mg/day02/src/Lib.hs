module Lib
    ( main
    ) where

import           Control.Monad   (guard)
import           Data.List       (permutations, uncons)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

main = print $ maximum signals

initial ::M.Map Int Int
initial = M.fromList $ zip [0..] input

-- phases1 :: [[Int]]
phases1 =  permutations [1,2,3,4,5]

phases  = do
  a <- [0..4]
  b <- [0..4]
  c <- [0..4]
  d <- [0..4]
  e <- [0..4]
  let phase = [a,b,c,d,e]
  guard $ S.fromList [0..4] == S.fromList phase
  pure phase

runThrough :: Int -> [Int] -> Int
runThrough input [] = input
runThrough input (phase:phases) =
  case run 0 [phase, input] initial of
    Just ([output], _) -> runThrough output phases
    Just ([],_)        -> error "empty output"
    Nothing            -> error "whoops"

signals = runThrough 0 <$> phases

getOp :: Int -> Maybe (Int -> Int -> Int)
getOp 1 = Just (+)
getOp 2 = Just (*)
getOp 7 = Just (\a b -> if a < b then 1 else 0)
getOp 8 = Just (\a b -> if a == b then 1 else 0)
getOp _ = Nothing

getIdx 5 a b idx = Just $ if a /= 0 then b else idx+3
getIdx 6 a b idx = Just $ if a == 0 then b else idx+3
getIdx _ _ _ _   = Nothing

inputValue = 5

run :: Int -> [Int] -> M.Map Int Int -> Maybe ([Int], M.Map Int Int)
run idx inputs acc = do
  case opCode of
    99 -> pure ([], acc)
    3 -> do
      pos <- M.lookup (idx + 1) acc
      (input, remaining) <- uncons inputs
      run (idx + 2) remaining $ M.insert pos input acc
    4 -> do
      val1 <- getVal idx acc 1 getmode1
      (outputs, acc') <- run (idx + 2) inputs acc
      pure (val1:outputs, acc')
    n | n `elem` [5,6] -> do
      val1 <- getVal idx acc 1 getmode1
      val2 <- getVal idx acc 2 getmode2
      idx' <- getIdx n val1 val2 idx
      run idx' inputs acc
    n | n `elem` [1,2,7,8] -> do
      val1 <- getVal idx acc 1 getmode1
      val2 <- getVal idx acc 2 getmode2
      pos3 <- M.lookup (idx + 3) acc
      op <- getOp n
      run (idx + 4) inputs $ M.insert pos3 (op val1 val2) acc
  where opCode = getOpcode idx acc

getVal idx acc shift getmode = do
      pos <- M.lookup (idx + shift) acc
      mode <- Just (getmode idx acc)
      if mode == 0 then M.lookup pos acc else Just pos

getOpcode :: Int -> M.Map Int Int -> Int
getOpcode idx acc =
    let mcode = M.lookup idx acc
    in case mcode of
       Just code -> code - ((div code 100) * 100)
       Nothing   -> 0

getmode1 :: Int -> M.Map Int Int -> Int
getmode1 idx acc =
    let mcode = M.lookup idx acc
    in case mcode of
       Just code -> div code 100 - (div code 1000) * 10
       Nothing   -> 0

getmode2 idx acc =
    let mcode = M.lookup idx acc
    in case mcode of
       Just code -> div code 1000 - (div code 10000) * 10
       Nothing   -> 0

input :: [Int]
input = [3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99]
