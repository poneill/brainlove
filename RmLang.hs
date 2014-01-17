{-- rm is a language intended to be compiled into the macro language
described in tobf.hs.  --}

-- The 
import Data.List
import qualified BrainLove as BL


type LN = Int
type Var = String
type Const = Int

data RmStatement = Alc Var
                   | Inc Var 
                   | Dec Var
                   | Set Var Const
                   | Jmp LN 
                   | Jz Var LN 
                   | In Var 
                   | Out Var
                   | Nop
                     deriving Show

addLineNumbers :: [RmStatement] -> RmProgram
addLineNumbers = zip [1..]

type RmLine = (LN,RmStatement)

type RmProgram = [RmLine]

sampleStatements = [Alc "a",
                      Alc "b",
                      Alc "c",
                      Set "a" 10,
                      Jz "a" 9,
                      Dec "a",
                      Inc "b",
                      Inc "c",
                      Jmp 4,
                      Nop]
                   
sampleProgram = addLineNumbers sampleStatements

--emitMacros :: RmProgram -> BL.Program
--emitMacros 

interpretStatement :: RmLine -> BL.Statements
interpretStatement (ln,statement) = case statement of 
  Alc var ->  [BL.allocate var, BL.zero pc, BL.increment pc']
  Inc var -> [BL.increment var, BL.zero pc, BL.increment pc']
  Dec var -> [BL.decrement var, BL.zero pc, BL.increment pc']
  Set var const -> [BL.set var const,BL.zero pc, BL.increment pc']
  --Jz var gotoLn -> [BL.if 
  where pc = "pc" ++ show ln
        pc' = "pc" ++ show (ln + 1)
  
    
allocatePCs :: RmProgram -> BL.Statements
allocatePCs rmProg = map BL.allocate pcs
                     where pcs = map (\x -> "pc" ++ show x ) lineNums
                           lineNums = map fst rmProg

