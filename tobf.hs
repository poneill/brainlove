type Instruction = Char
type Program = [Instruction]
type Address = Int
type Var = String
type VarTable = [(Var,Address)]
data Context = Context { program :: Program
                       , varTable :: VarTable
                       }
               deriving Show
type Statement = Context -> Context
type Statements = [Statement]

initContext = Context [] []

currentPos :: Program -> Int
currentPos program = foldr stack 0 program
    where stack char pos
              | char == '>' = pos + 1
              | char == '<' = pos - 1
              | char == '|' = 0
              | otherwise = pos

shift :: Int -> Context -> Context
shift offset context = context{program = program context ++ replicate (abs offset) symbol}
    where symbol = if offset > 0 then '>' else '<'

gotoZero :: Context -> Context
gotoZero context  = shift (-(currentPos (program context))) context

write :: Program -> Context -> Context
write prog context = context{program = program context ++ prog}

doStatements :: Statements -> Context -> Context
doStatements = foldr1 (.) . reverse

writeProgram :: Statements -> Context
writeProgram statements = liftProgram optimize (doStatements statements initContext)

writeProgramVerbose :: Statements -> Context
writeProgramVerbose statements = doStatements statements initContext

(<-.) :: Var -> Var -> Context -> Context
(<-.) b a = doStatements [ goto a
                         , write "[-"
                         , goto b
                         , write "+"
                         , goto a
                         , write "]"
                         , gotoZero
                         ]

(<--.) :: [Var] -> Var -> Context -> Context
(<--.) bs a context = doStatements (start ++ copies ++ stop) context
    where start = [goto a, write "[-"]
          copies = concat [[goto b, write "+"] | b <- bs]
          stop = [goto a, write "]", gotoZero]

goto :: Var -> Context -> Context
goto a context = doStatements statements context
    where statements = [ gotoZero
                       , shift (addressOf a)
                       ]
          addressOf = flip getAddress (varTable context)

(=.) :: Var -> Var -> Context -> Context
(=.) b a context = doStatements statements context
    where statements = [ allocate temp
                       , (<--.) [temp,b] a
                       , (<-.) a temp
                       , deallocate temp
                       ]
          temp = uniqueVar context

zero :: Var -> Context -> Context
zero var = doStatements [ goto var
                        , write "[-]"
                        , gotoZero
                        ]

set :: Var -> Int -> Context -> Context
set var const = doStatements [ zero var
                             , goto var
                             , write $ replicate const '+'
                             , gotoZero
                             ]

uniqueVar :: Context -> Var
uniqueVar context = head $ dropWhile (`elem` vars) allVars
    where allVars = freeSemiRing ['a'..'z']
          vars = map fst varTab
          varTab = varTable context

liftVarTable :: (VarTable -> VarTable) -> Context -> Context
liftVarTable f context = context{varTable = f (varTable context)}

liftProgram :: (Program -> Program) -> Context -> Context
liftProgram f context = context{program = f (program context)}

allocate :: Var -> Context -> Context
allocate = liftVarTable . allocate'

allocate' :: Var -> VarTable -> VarTable
allocate' var varTable = varTable ++ [(var, nextFree varTable)]

deallocate :: Var -> Context -> Context
deallocate = liftVarTable . deallocate'

deallocate' :: Var -> VarTable -> VarTable
deallocate' var = filter (\(v,a) -> v /= var)

nextFree :: VarTable -> Address
nextFree varTable = head $ dropWhile (flip elem (map snd varTable)) [0..]

getAddress :: Var -> VarTable -> Address
getAddress var varTable = (snd . head) $ filter (\(v,a) -> v == var) varTable

optimize :: Program -> Program
optimize program = reverse $ annihilate program ""

annihilate :: Program -> String -> Program
annihilate "" hold = hold
annihilate ('<':prog) ('>':hold) =  annihilate prog hold
annihilate ('>':prog) ('<':hold) = annihilate prog hold
annihilate ('+':prog) ('-':hold) =  annihilate prog hold
annihilate ('-':prog) ('+':hold) =  annihilate prog hold
annihilate (p:prog) hold = annihilate prog (p:hold)

freeSemiRing xs = concat $ iterate (\ys -> [x ++ y | x <- xs', y<- ys]) xs'
    where xs' = map return xs