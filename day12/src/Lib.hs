{-# LANGUAGE DataKinds #-}

module Lib
    ( run
    ) where

import           Data.List as L

type Coor = [Int]
type Vel = [Int]
type Moon = (Coor, Vel)

initCoor = [[-8, -18, 6], [-11, -14, 4], [8, -3, -10], [-2, -16, 1]]
tinitCoor = [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]
initVel = [[0,0,0],[0,0,0],[0,0,0],[0,0,0]]
initMoons :: [Moon]
initMoons = zip initCoor initVel

-- run :: IO ()
run = print $ findIndex (\i -> i == True) $ repeated <$> iterate moveTheMoons initMoons

repeated moonsStates = elem (last moonsStates) (L.init moonsStates)

firstStar = foldl (\acc moon -> acc + (calcE moon)) 0 $ last $ take 1001 $ iterate moveTheMoons initMoons

calcE (c, v) = pE * kE
    where pE = sum $ abs <$> c
          kE  = sum $ abs <$> v

moveTheMoons moons = aplVel <$> aplGravity moons 0

aplVel (coors, vels) = (zipWith (+) coors vels, vels)

aplGravity (me:others) c
  | c == 4    = me:others
  | otherwise = aplGravity (others ++ [(myCoors, myNewVels)]) (c+1)
  where (myCoors, myVels) = me
        myNewVels = (newVels me others)

newVels :: Moon -> [Moon] -> Vel
newVels (myCoors, myVels) others =
  zipWith (+) [velsChange 0, velsChange 1, velsChange 2] myVels
  where
        velsChange n = velChange (myCoors!!n) (otherXCoors n)
        otherXCoors n = (\(otherCoors, _) -> otherCoors!!n) <$> others


velChange :: Int -> [Int] -> Int
velChange myX otherXs =
  foldl ch 0 otherXs
  where ch acc oX | oX > myX = acc + 1
                  | oX < myX = acc - 1
                  | oX == myX = acc
