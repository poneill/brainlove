import System.Environment
import System.IO
import Debug.Trace
import Data.Char

type Transition = (State -> State)
type IOTransition = (State -> IO State)
type Instruction = Char
type Program = [Instruction]
type ProgramPtr = Int
type Memory = [Int]
type MemoryPtr = Int

data State = ST { program :: Program 
                , programPtr :: ProgramPtr 
                , memory :: Memory 
                , memoryPtr :: MemoryPtr 
                }
           deriving Show

data AltMem = AM {
  
  }
getTransition :: Instruction -> IOTransition
getTransition c = case c of 
  '+'       -> return . incRegister
  '-'       -> return . decRegister
  '>'       -> return . incMemoryPtr
  '<'       -> return . decMemoryPtr
  '['       -> return . leftBracket
  ']'       -> return . rightBracket
  ','       -> accept
  '.'       -> output
  otherwise -> return .nonce
  
halted :: State -> Bool
halted st = (programPtr st) == length (program st)

updateRegister :: Int -> Transition
updateRegister sgn = (\st -> let memory' = (take mp mem ++ 
                                            [mem!! mp + sgn] ++ 
                                            drop (mp + 1) mem)
                                 mp = memoryPtr st 
                                 mem = memory st
                             in st {memory = memory'})

updateMemoryPtr :: Int -> Transition
updateMemoryPtr sgn = \st -> st {memoryPtr = (memoryPtr st) + sgn}

registerContent :: State -> Int
registerContent st = (memory st) !! (memoryPtr st)

incRegister :: Transition
incRegister = updateRegister 1
decRegister :: Transition
decRegister = updateRegister (-1)
incMemoryPtr :: Transition
incMemoryPtr = updateMemoryPtr 1
decMemoryPtr :: Transition
decMemoryPtr = updateMemoryPtr (-1)
leftBracket :: Transition
leftBracket st  
  | register == 0 = jump st
  | otherwise = st
    where register = registerContent st
rightBracket :: Transition
rightBracket st  
  | register /= 0 = jump st
  | otherwise = st
    where register = registerContent st

accept :: State -> IO State
accept st = do
  line <- getLine
  let newVal = (ord . head) line
  return ((updateRegister (newVal - (currentRegister st))) st)
  
output :: State -> IO State
output st = do
  putStr $ (return . chr . currentRegister) st
  return st
  
nonce :: State -> State
nonce = id

jump :: Transition
jump st =  st {programPtr = findMatching (program st) (programPtr st)}

findMatching :: Program -> ProgramPtr -> ProgramPtr
findMatching program programPtr = findBracket program programPtr bracket (-1)
    where bracket = program !! programPtr

findBracket :: Program -> ProgramPtr -> Instruction -> Int -> ProgramPtr
findBracket program programPtr bracket depth 
  | matchesBracket current bracket && depth == 0 = programPtr
  | matchesBracket current bracket               = changeDepth (depth - 1)
  | sameBracket current bracket                  = changeDepth (depth + 1)
  | otherwise                                        = changeDepth depth
    where changeDepth = findBracket program (programPtr + direction) bracket
          current = program !! programPtr
          direction = if bracket == '[' then 1 else -1

matchesBracket :: Instruction -> Instruction -> Bool
matchesBracket x y = x == '[' && y == ']' || y == '[' && x == ']'

sameBracket :: Instruction -> Instruction -> Bool
sameBracket = (==)

currentInstruction :: State -> Instruction
currentInstruction st = (program st) !! (programPtr st)

currentRegister :: State -> Int
currentRegister st = (memory st) !! (memoryPtr st)

advanceInstruction :: Transition
advanceInstruction st  
  | halted st = st 
  | otherwise = st{ programPtr = (programPtr st) + 1}

step :: IOTransition
step st = fmap advanceInstruction ((getTransition $ currentInstruction st) st)

run :: IOTransition
--run st | trace ("run " ++ show st ++ " ") False = undefined
run st = do
  newSt <- step st
  if (halted newSt) 
    then return newSt
    else run newSt

runProg :: Program -> IO State
runProg prog = run st
  where st = ST prog 0 [0 | i <- [1..]] 0
        
main = do 
  args <- getArgs
  let program = head args
  contents <- readFile program
  runProg contents