module Lib
    ( run
    , runInit
    ) where

run :: Int -> Int -> [Int] -> ([Int], Int, Int, Int)
run input address memory
    | opcode address memory == 1    = run input (address+4) (execute address memory (+))
    | opcode address memory == 2    = run input (address+4) (execute address memory (*))
    | opcode address memory == 5    = run input (jumpTo address memory (/=)) memory
    | opcode address memory == 6    = run input (jumpTo address memory (==)) memory
    | opcode address memory == 7    = run input (address+4) (compareAndSave address memory (<))
    | opcode address memory == 8    = run input (address+4) (compareAndSave address memory (==))
    | opcode address memory == 3    = run input (address+2) (replaceNth (memory!!(address+1)) input memory)
    | opcode address memory == 4    = (memory, address + 2, myread (address+1) memory (mode1 address memory), 4)
    | opcode address memory == 99   = (memory, address, input, 99)
    | otherwise                     = (memory, address, 999999999999999, 0)

runInit :: [Int] -> Int -> [Int] -> ([Int], Int, Int, Int)
runInit input address memory
    | opcode address memory == 1    = runInit input (address+4) (execute address memory (+))
    | opcode address memory == 2    = runInit input (address+4) (execute address memory (*))
    | opcode address memory == 5    = runInit input (jumpTo address memory (/=)) memory
    | opcode address memory == 6    = runInit input (jumpTo address memory (==)) memory
    | opcode address memory == 7    = runInit input (address+4) (compareAndSave address memory (<))
    | opcode address memory == 8    = runInit input (address+4) (compareAndSave address memory (==))
    | opcode address memory == 3    = runInit (tail input) (address+2) (replaceNth (memory!!(address+1)) (head input) memory)
    | opcode address memory == 4    = (memory, address + 2, myread (address+1) memory (mode1 address memory), 4)
    | opcode address memory == 99   = (memory, address, head input, 99)
    | otherwise                     = (memory, address, 999999999999, 0)

execute address memory fnc =
    let m1 = (mode1 address memory)
        m2 = (mode2 address memory)
        val1 = (myread (address+1) memory m1)
        val2 = (myread (address+2) memory m2)
        nth = (resultAddress address memory)
    in replaceNth nth (fnc val1 val2) memory

jumpTo address memory fnc
    | fnc val1 0 = val2
    | otherwise = address + 3
    where val1 = myread (address+1) memory (mode1 address memory)
          val2 = myread (address+2) memory (mode2 address memory)

compareAndSave address memory fnc =
    replaceNth val3 result memory
    where val1 = myread (address+1) memory (mode1 address memory)
          val2 = myread (address+2) memory (mode2 address memory)
          val3 = memory!!(address+3)
          result
                | fnc val1 val2 = 1
                | otherwise = 0

opcode :: Int -> [Int] -> Int
opcode address memory =
    let code = memory!!address
    in code - ((div code 100) * 100)

mode1 address memory =
    let code = memory!!address
    in div code 100 - (div code 1000) * 10

mode2 address memory =
    let code = memory!!address
    in div code 1000 - (div code 10000) * 10

myread :: Int -> [Int] -> Int -> Int
myread address memory mode
    | mode == 0 = memory!!(memory!!(address))
    | mode == 1 = memory!!(address)
    | otherwise = 0

resultAddress :: Int -> [Int] -> Int
resultAddress address memory =
    memory!!(address+3)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs
