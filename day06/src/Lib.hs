{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( result
    ) where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.IO

data Planet a = Moon a | Planet a [Planet a] deriving Show

result :: IO ()
result= do
        orbitsData <- lines <$> readFile "src/input2.txt"
        print $ countOrbits orbitsData

--countOrbits :: [String] -> Planet String
countOrbits orbitsData =
    let orbits :: [[String]]
        orbits = map (\orbit -> splitOn ")" orbit) orbitsData
        firstOrbit = findCOM orbits
        parent = head firstOrbit
        child = firstOrbit!!1
        nextPlanet = moonOrPlanet (findNextOrbit orbits child) orbits child
   in allDepths $ Planet parent [nextPlanet]

moonOrPlanet :: [[String]]-> [[String]] -> String -> Planet String
moonOrPlanet [] _ child           = Moon child
moonOrPlanet nextOrbits orbits child  = Planet child (map (\nextOrbit -> moonOrPlanet (findNextOrbit orbits (nextOrbit!!1)) orbits (nextOrbit!!1)) nextOrbits)

findNextOrbit :: [[String]] -> String -> [[String]]
findNextOrbit orbits child = filter (\nextOrbit -> (head nextOrbit) == child) orbits

allDepths :: Planet a -> Int
allDepths (Moon a)           = planetDepth (Moon a)
allDepths (Planet a planets) = (planetDepth (Planet a planets)) + (sum $ map (\planet -> allDepths planet) planets)

planetDepth :: Planet a -> Int
planetDepth (Moon a)           = 0
planetDepth (Planet _ planets) = sum $ map (\n -> n+1) $ map (\planet -> planetDepth planet) planets

findCOM orbits = fromJust $ find (\orbit -> (head orbit) == "COM") orbits
