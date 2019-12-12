{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Robot
    ( run
    ) where

import           Data.Set     as S
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Intcode      as I

initIdx = 0
initBase = 0
initVisitedFields = S.empty
initWhitePanels = S.singleton (0,0)
initCoor = (0,0)
initDir = 0
type Coor = (Int, Int)
type Dir = Int
type Robot = (Coor, Dir)

run = do
    program :: [Int] <- fmap read . fmap T.unpack . T.splitOn "," <$> TIO.readFile "src/input.txt"
    let prg = program ++ (replicate 10000 0)
    let (count, pic) = work (prg, initIdx, initBase) initVisitedFields initWhitePanels (initCoor, initDir)
    let maxX = maximum $ S.map (\(x,y) ->x) $ pic
    let minX = minimum $ S.map (\(x,y) ->x) $ pic
    let maxY = maximum $ S.map (\(x,y) ->y) $ pic
    let minY = minimum $ S.map (\(x,y) ->y) $ pic
    let width = abs maxX + abs minX +1
    let heigth = abs maxY + abs minY +1
    let picS = S.map (\(x,y) -> (x-minX,-(y-maxY))) $ pic
    let canvas = (\_ -> replicate width ' ') <$> replicate heigth ' '
    let registration = draw canvas picS
    mapM putStrLn $ registration

draw :: [[Char]] -> S.Set Coor -> [[Char]]
draw canvas pic = yellow <$> zip canvas [0..]
    where yellow (row, y) = green y <$> zip row [0..]
          green y (_, x) = pink  pic (x,y)
          pink pic point =
            case isWhite of
              True  -> 'O'
              False -> ' '
            where isWhite = S.member point pic

work :: I.Program -> S.Set Coor-> S.Set Coor-> Robot -> (Int, Set Coor)
work prg vf wp (currentCoor, currentDir) =
  let newVF = S.insert currentCoor vf
      currentColor = panelColor wp currentCoor

      (color, prg2) = I.run prg currentColor
      finished = color == 99

  in case finished of
    True -> (length vf, wp)
    False -> let (rotation, prg3) = I.run prg2 0
                 newWF = paintPanel wp currentCoor color
                 newDir = mod (currentDir - 1 + (rotation * 2)) 4
                 newCoor = move currentCoor newDir
              in work prg3 newVF newWF (newCoor, newDir)


move :: Coor -> Dir -> Coor
move (x,y) newDir =
  case newDir of
     0 -> (x, y+1)
     1 -> (x+1, y)
     2 -> (x, y-1)
     3 -> (x-1, y)

panelColor :: Set Coor -> Coor -> Int
panelColor wp square =
  case isWhite of
    True  -> 1
    False -> 0
  where isWhite = S.member square wp

paintPanel :: Set Coor -> Coor -> Int -> Set Coor
paintPanel wp square color =
  case color of
    0 -> S.delete square wp
    1 -> S.insert square wp
