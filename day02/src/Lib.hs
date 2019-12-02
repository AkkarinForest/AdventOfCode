module Lib
    ( result
    ) where

result :: IO ()
result = print $ fixGravity

fixGravity :: Maybe Int
fixGravity = lookup theEnd [(findOutput noun verb, answer noun verb) | noun <- [0..99], verb <- [0..99]]

theEnd :: Int
theEnd = 19690720

findOutput :: Int -> Int -> Int
findOutput noun verb = head $ run 0 $ setup initialProgram noun verb

answer :: Int -> Int -> Int
answer noun verb = 100 * noun + verb

setup :: [Int] -> Int -> Int -> [Int]
setup program noun verb =
    (head program) : noun : verb : (drop 3 program)

run :: Int -> [Int] -> [Int]
run address memory
    | opcode address memory == 99   = memory
    | opcode address memory == 1    = run (address+4) (execute address memory (+))
    | opcode address memory == 2    = run (address+4) (execute address memory (*))
    | otherwise                     = [-1]

execute address memory fnc = replaceNth (resultAddress address memory) (fnc (value1 address memory) (value2 address memory)) memory

opcode :: Int -> [Int] -> Int
opcode address memory =
    memory!!address

value1 :: Int -> [Int] -> Int
value1 address memory =
    memory!!(memory!!(address+1))

value2 :: Int -> [Int] -> Int
value2 address memory =
    memory!!(memory!!(address+2))

resultAddress :: Int -> [Int] -> Int
resultAddress address memory =
    memory!!(address+3)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

initialProgram = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,13,19,23,2,23,9,27,1,6,27,31,2,10,31,35,1,6,35,39,2,9,39,43,1,5,43,47,2,47,13,51,2,51,10,55,1,55,5,59,1,59,9,63,1,63,9,67,2,6,67,71,1,5,71,75,1,75,6,79,1,6,79,83,1,83,9,87,2,87,10,91,2,91,10,95,1,95,5,99,1,99,13,103,2,103,9,107,1,6,107,111,1,111,5,115,1,115,2,119,1,5,119,0,99,2,0,14,0]
