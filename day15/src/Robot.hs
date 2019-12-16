{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Robot
    ( run
    ) where

import           Control.Concurrent
import           Data.List           as L
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Intcode             as I
import qualified System.Console.ANSI as S

inf = 999
padd = 3

type Coor = (Int, Int)
initCoor :: Coor = (21,25)
type SectionMap = [[(Char,Int)]]
l = replicate 20 (' ', inf)
ml = l ++ [('B',inf)] ++ l
il = replicate 41 (' ', inf)
ib = replicate 25 il
initMap :: SectionMap = ib ++ [ml] ++ ib
type Dir = Int
initDir :: Dir = 1

run :: IO ()
run = do
  input :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> TIO.readFile "src/input.txt"
  let prg = (input ++ (replicate 1000 0), 0,0,0)
  let (rCoor, prg', dir, sectionMap) = startSearch prg
  display rCoor prg' dir sectionMap 1

display :: Coor -> I.Program -> Dir -> SectionMap -> Int -> IO ()
display rCoor prg dir sectionMap stepCount = do
  let (rCoor', prg', dir', sectionMap', obj', stepCount') = search rCoor prg dir sectionMap stepCount
  -- mapM putStrLn $ drawRobot rCoor' $ (\l-> (\(c, i) ->c) <$> l) <$> sectionMap'
  mapM_ print $ (\l -> intercalate " " $ (\x -> toSteps x) <$> l) <$> drawRobot2 rCoor' sectionMap'
  print (rCoor', dir', obj')
  threadDelay 100
  S.clearScreen
  case obj' of
      2                  -> display rCoor' prg' dir' sectionMap' stepCount'
      n | n `elem` [0,1] -> display rCoor' prg' dir' sectionMap' stepCount'
      n                  -> error $ "object out of range in v: " ++ show n


drawRobot2 :: (Int, Int) -> [[(Char, Int)]] -> [[(Char, Int)]]
drawRobot2 (x,y) sectionMap =
  replaceNth y newLine sectionMap
  where newLine = replaceNth x ('@', c) (sectionMap!!y)
        (s, c) = (sectionMap!!y)!!x

toSteps (s, c) =
  (\x -> [s] ++ replicate (padd - length x) '0' ++ x) (show c)

startSearch :: I.Program -> (Coor, I.Program, Dir, SectionMap)
startSearch prg =
  (adjustedRCoor, prg', dir, sectionMap)
  where   dir = findNextDir initDir obj
          (adjustedRCoor, sectionMap, _) = mark rCoor objCoor initMap obj 0
          (rCoor, objCoor, prg', obj) = move (initCoor, prg) initDir

search :: Coor -> I.Program -> Dir -> SectionMap -> Int -> (Coor, I.Program, Dir, SectionMap, Int, Int)
search rCoor prg dir sectionMap stepCount =
  (adjustedRCoor', prg', dir', sectionMap', obj', stepCount')
  where
        dir' = findNextDir dir obj'
        (adjustedRCoor', sectionMap', stepCount') = mark rCoor' objCoor' sectionMap obj' stepCount
        (rCoor', objCoor', prg', obj') = move (rCoor, prg) dir

findNextDir :: Int -> Int -> Int
findNextDir dir obj =
  case obj of
    1                  -> case dir of
                            1 -> 3 -- N -> W
                            4 -> 1 -- E -> N
                            2 -> 4 -- S -> E
                            3 -> 2 -- w -> S
    n | n `elem` [0,2] -> case dir of
                            1 -> 4 -- N -> E
                            4 -> 2 -- E -> S
                            2 -> 3 -- S -> W
                            3 -> 1 -- w -> N
    n                  -> error $ "object out of range in findNextDir: " ++ show n

move :: (Coor, I.Program) -> Int -> (Coor, Coor, I.Program, Int)
move (coor, prg) dir =
  (robotCoor, objCoor, newPrg, obj)
  where robotCoor = moveRobot coor obj dir
        objCoor = moveObj coor dir
        (_,_,_,obj) = newPrg
        newPrg = I.processInput prg dir

mark :: Coor -> Coor -> SectionMap -> Int -> Int -> (Coor, SectionMap, Int)
mark robotCoor objCoor sectionMap obj stepCount =
  case (adjMap!!y!!x) of
    ('B',_) -> (adjRCoor, adjMap, stepCount')
    cc      -> (adjRCoor, replaceNth y newLine adjMap, stepCount')
  where newLine = replaceNth x (obj', stepCount') (adjMap!!y)
        stepCount' = countNewStepCount obj stepCount
        obj' = objSign obj
        (x, y) = adjOCoor
        adjRCoor = adjustCoor objCoor robotCoor
        adjOCoor = adjustCoor objCoor objCoor
        adjMap = adjustMap sectionMap objCoor

countNewStepCount obj sc =
  case obj of
    0 -> sc
    n | n `elem` [1,2] -> sc + 1
    n                  -> error $ "object out of range in countNewStepCount: " ++ show n


adjustCoor :: Coor -> Coor -> Coor
adjustCoor (x,y) (ox, oy) = (nx,ny)
  where nx = adjustCoorX x ox
        ny = adjustCoorX y oy

adjustCoorX :: Int -> Int -> Int
adjustCoorX x ox = nx
  where nx | x<0 = ox - x
           | otherwise = ox

adjustMap :: SectionMap -> Coor -> SectionMap
adjustMap sectionMap (x,y) =
  adjustMapX x $ adjustMapY y sectionMap

adjustMapX :: Int -> SectionMap -> SectionMap
adjustMapX x sectionMap
  | x < 0 = (\l -> newPointsWest ++ l) <$> sectionMap
  | x >= maxX = (\l -> l ++ newPointsEast) <$> sectionMap
  | otherwise = sectionMap
  where
        maxX = maximum $ length <$> sectionMap
        newPointsEast = newPoints (x - maxX +1)
        newPointsWest = newPoints (abs x)
        newPoints n = replicate n (' ',inf)

adjustMapY :: Int -> SectionMap -> SectionMap
adjustMapY y sectionMap
  | y < 0 = newLinesNorth ++ sectionMap
  | y >= maxY = sectionMap ++ newLinesSouth
  | otherwise = sectionMap
  where
        maxY = length sectionMap
        maxX = maximum $ length <$> sectionMap
        newLinesNorth = replicate (abs y) newLine
        newLinesSouth = replicate (y - maxY + 1) newLine
        newLines n = replicate n newLine
        newLine = replicate maxX (' ',inf)

objSign obj =
  case obj of
    0 -> '*'
    1 -> '.'
    2 -> '$'
    n -> error $ "object out of range in objSign: " ++ show n

moveObj (x,y) dir =
  case dir of
    1 -> (x,y-1)
    2 -> (x,y+1)
    3 -> (x-1,y)
    4 -> (x+1,y)
    n -> error $ "dir out of range in moveObj: " ++ show n

moveRobot (x,y) obj dir =
  case obj of
    0                  -> (x,y)
    n | n `elem` [1,2] -> moveObj (x,y) dir
    n                  -> error $ "object out of range in moveRobot: " ++ show n


drawRobot (x,y) sectionMap =
  replaceNth y newLine sectionMap
  where newLine = replaceNth x '@' (sectionMap!!y)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs
