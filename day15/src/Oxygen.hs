{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Oxygen
    ( run
    ) where

import           Control.Concurrent
import           Data.List           as L
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Intcode             as I
import qualified System.Console.ANSI as S
import qualified Robot as R
import qualified Data.Maybe as M
import Data.List.Utils (replace)

type Coor = (Int, Int)
type SectionMap = [[(Char,Int)]]
type Dir = Int
type Labirynth = [[Char]]
--[(39,3)]

-- run :: IO ()
run = do
  labirynth <- addMissingWalls . fmap T.unpack . T.splitOn "\n" <$> TIO.readFile "src/labirynth.txt"
  -- labirynth <- addMissingWalls R.generatLabirynth
  let sCoor = [findSCoor labirynth]
  print $ yellow labirynth sCoor 0
  -- mapM putStrLn $ yellow labirynth sCoor 0

yellow :: Labirynth -> [Coor] -> Int -> Int
yellow lab coors c =
  case end of
    True -> c
    False -> yellow lab' oo (c+1)
  where
    oo = allOxygenated lab'
    lab' = spreadO lab emptyNeibh
    end = isEnd emptyNeibh
    emptyNeibh = filterEmpty lab $ allneigh coors

spreadO :: Labirynth -> [Coor] -> Labirynth
spreadO lab coor = foldl (\lab p -> replaceXY p 'O' lab) lab coor

readSigns :: Labirynth -> [Coor] -> [Char]
readSigns labirynth = map (\(x,y) -> labirynth!!y!!x)

isEnd :: [Coor] -> Bool
isEnd = (==) 0 . length

allOxygenated :: Labirynth -> [Coor]
allOxygenated lab = concat $ map (\y -> map (\x -> (x,y)) (elemIndices 'O' (lab!!y))) yy
  where yy = findIndices (\y -> elem 'O' y ) lab

filterEmpty :: Labirynth -> [Coor] -> [Coor]
filterEmpty lab = filter (\(x,y) -> (lab!!y!!x) == '.')

allneigh coors =
  concat $ map neighbours coors

neighbours :: Coor -> [Coor]
neighbours (x,y) = [(x-1, y), (x+1, y), (x, y+1), (x,y-1)]

findSCoor :: Labirynth -> Coor
findSCoor labirynth =
  (x, y)
  where
        x = M.fromJust $ elemIndex '$' $ labirynth!!y
        y = M.fromJust $ findIndex hasS labirynth
        hasS = elem '$'

replaceXY :: (Int, Int) -> a -> [[a]] -> [[a]]
replaceXY (x,y) a la =
  replaceNth y line' la
  where
    line' = replaceNth x a line
    line = la!!y

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

addMissingWalls :: Labirynth -> Labirynth
addMissingWalls = map (\l -> replace " " "*" l)
