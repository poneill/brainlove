{- Brainlove is a psuedo-assembly-like macro language that compiles to
brainfuck (as well as the name of the compiler for that language.)  It
is also a learning project for Haskell.  Accordingly, some moves are
made far more explicitly than they could be.  Comments and suggestions
are always welcome.

brainlove converts sequences of statements into brainfuck code.  The
Brainlove language, however, is not formally specified; rather, it is
totally made up as I go along.  The idea at present is to try to
formulate natural macros that will cleanly bootstrap up into something
like assembly or even BASIC.  Currently, the only implemented features
are memory allocation, various forms of variable assignment and an add
instruction.  

Let's start out by defining some type synonyms to motivate ourselves.-}

type Instruction = Char
-- An instruction is a single character of brainfuck code.
type Program = [Instruction]
-- A program, naturally, is a list of brainfuck instructions.
type Address = Int
-- An address is a location on the brainfuck tape, represented by an integer.
type Var = String
-- A brainlove variable is just a string.  We won't place any
-- restrictions on what constitutes a valid identifier.  We will just
-- assume that the user would never try to use brainlove keywords or
-- strings of punctuation or anything like that.  That would be rude.
type VarTable = [(Var,Address)]
-- A VarTable is a mapping from variables to addresses.  There is a
-- function in the Standard Prelude called 
--
-- lookup :: a -> [(a,b)] -> Maybe b 
--
-- which gives you an idea of how the VarTable type will be used here.
-- Lookup anticipates failure and wraps its result in the Maybe monad.
-- Here we are going balls to the wall and assuming that the user will
-- never reference unallocated variables.
 
data Context = Context { program :: Program 
                       , varTable :: VarTable } 
               deriving Show 

-- Contexts are datatypes consisting of a Program and a VarTable.  As
-- we pass through a brainlove program, the context is just the
-- brainfuck code emitted so far, along with a record of what
-- memory is currently in play.
type Statement = Context -> Context 
-- A statement (of brainlove) is a function from context to context.  
type Statements = [Statement]
-- The type Statements is just a list of type Statement, which you can
-- just think of as a brainlove program.
initContext = Context [] []
-- The simplest context you can have is just an empty program and an empty varTable.
currentPos :: Program -> Int
currentPos program = foldr stack 0 program
    where stack char pos
              | char == '>' = pos + 1
              | char == '<' = pos - 1
              | char == '|' = 0
              | otherwise = pos
-- Often it will be important to know where we are on the brainfuck
-- tape.  currentPos tells us.  I have not yet decided whether it is
-- worthwhile to implement the '|' instruction, which is a breadcrumb
-- to the compiler that the tape head is currently at the 0 position.
-- (Most brainfuck interpreters would treat those as comments, and in
-- any case it would be trivial to remove them on the final pass.  For
-- now, it's left in.

shift :: Int -> Context -> Context
shift offset context = context{program = program context ++ replicate (abs offset) symbol}
    where symbol = if offset > 0 then '>' else '<'
-- The shift function just moves the tape head left or right by the specified offset
gotoZero :: Context -> Context
gotoZero context  = shift (-(currentPos (program context))) context
-- gotoZero returns the tape head to the zero position.  This is
-- actually one of the most important functions, since the compiler
-- relies on a sort of "dead-reckoning" system to access memory: since
-- there's no way of directly accessing the position of the tape head
-- at run-time, we must keep track of it at compile-time by repeatedly
-- "tagging home base" between statements.  Fortunately these
-- redundant instructions are trivially optimizable; otherwise they
-- could actually change the big O of brainlove algorithms, since
-- variable access would be linear in the number of allocated
-- variables.

write :: Program -> Context -> Context
write code context = context{program = program context ++ code}
-- The write function just emits some raw brainfuck code into the
-- current context

doStatements :: Statements -> Context -> Context
doStatements = foldr1 (.) . reverse
{- The doStatements function attempts to corral several of Haskell's
distinguishing features, chiefly higher-order functions and currying,
in order to create a sort of DSL in which brainlove programs can be
written, so it is worth explaining in detail.  Notice that every
brainlove macro takes a Context as its final argument and returns
another Context.  By partially applying their arguments, we obtain
functions of type Context -> Context.  Recall the following types:

allocate :: Var -> Context -> Context
set :: Var -> Int -> Context -> Context
(=.) :: Var -> Var -> Context -> Context

Now if we write:

prog = [ allocate "a"
       , allocate "b"
       , set "a" 10
       , (=.) b a
       ]

then all of the enlisted functions have been partially applied up to
Context, so their type is Context -> Context and prog has type
[Context -> Context].  Note also that prog, funky punctuation aside,
is essentially a human-readable program.  Using reverse and foldr
(with composition as our operation), we can telescope the list of
functions to obtain a final function of type Context -> Context which
is the result of applying each of the statements successively.  (We
must take care to reverse our list first, since we want the first
function written to be the innermost function in the composition.
Finally, applying the composed function to an initial context, we get
a final context.  This results in the final desirable property that
doStatements itself can be partially applied to yield a function of
type Context -> Context, so we can compose large programs from smaller
programs in a natural way.-}

writeProgram :: Statements -> Context
writeProgram statements = liftProgram optimize (doStatements statements initContext)

writeProgramVerbose :: Statements -> Context
writeProgramVerbose statements = doStatements statements initContext

(<-.) :: Var -> Var -> Context -> Context --Dump a into b: add a to b, destroying a
(<-.) b a = doStatements [ goto a
                         , write "[-"
                         , goto b
                         , write "+"
                         , goto a
                         , write "]"
                         , gotoZero
                         ]

(<--.) :: [Var] -> Var -> Context -> Context --dump a into bs, destroying a
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

(<=.) :: Var -> Var -> Context -> Context --add a to b, preserving a
(<=.) b a context = doStatements statements context
    where statements = [ allocate temp
                       , (<--.) [temp,b] a
                       , (<-.) a temp
                       , deallocate temp
                       ]
          temp = uniqueVar context

(=.) :: Var -> Var -> Context -> Context --set b equal to a, preserving a
(=.) b a = doStatements [ zero b
                        , (<=.) b a
                        ] 

add :: Var -> Var -> Var -> Context -> Context --add a + b, store result in c
add c a b = doStatements [ (=.)  c a --set c to a
                         , (<=.) c b --then safely add b to c
                         ]

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

