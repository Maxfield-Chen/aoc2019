module IntCode
  ( module IntCode
  )
where

import           Text.ParserCombinators.Parsec
import           Data.List
import           Data.Char                      ( digitToInt )
import           Control.Monad.State

data IntState = IntState { pc :: PC,
                           relBase :: PC
                         , code :: [Op]
                         , input :: [Op]
                         , output :: [Op]
                         , status :: Status
                         }

instance Show IntState where
  show s =
    "IntState { \n pc     : "
      ++ show (pc s)
      ++ "\n relBase: "
      ++ show (relBase s)
      ++ "\n code   : "
      ++ show (take (pc s) (code s))
      ++ " | "
      ++ (show . take maxCodeLen . drop (pc s)) (code s)
      ++ "\n input  : "
      ++ show (input s)
      ++ "\n output : "
      ++ show (output s)
      ++ "\n status : "
      ++ show (status s)
      ++ " }"

type Op = Int
type PC = Int
data Mode = Position | Immediate | Relative deriving (Show, Eq)
data Status = Halt | Input | Running deriving (Show, Eq)

maxOps = 3
maxCodeLen = 50
fileName = "/home/nihliphobe/projects/haskell/aoc2019/Day11/data/input.txt"
emptyIntState = IntState 0 0 [99] [] [] Halt

pOps :: Parser [Op]
pOps = sepBy (choice [pOp, pOpMinus]) comma

pOp :: Parser Op
pOp = do
  digits <- many1 digit
  return (read digits)

pOpMinus :: Parser Op
pOpMinus = do
  sign   <- char '-'
  digits <- many1 digit
  let value = read digits in return (-value)

comma :: Parser Char
comma = char ','

parseOps :: String -> Either ParseError [Op]
parseOps = parse pOps "(unknown)"

parseModes :: [Mode] -> String -> [Mode]
parseModes ret (ms : xs) = parseModes (mode : ret) xs
 where
  mode | ims == 1  = Immediate
       | ims == 2  = Relative
       | otherwise = Position
  ims = digitToInt ms

parseModes ret m = reverse ret

rpad :: Int -> Mode -> [Mode] -> [Mode]
rpad n mode ret | length ret < n = rpad n mode (ret ++ [mode])
                | otherwise      = ret

opToInstruction :: Op -> (Op, [Mode])
opToInstruction i = (op, rpad maxOps Position modes)
 where
  s     = show i
  rs    = reverse s
  op    = read (reverse (take 2 rs)) :: Int
  modes = parseModes [] (drop 2 rs)

intStateToPosition :: IntState -> [Op]
intStateToPosition s = drop (pc s) (code s)

replaceOp :: PC -> Op -> [Op] -> [Op]
replaceOp n x code = take n code ++ [x] ++ drop (n + 1) code

evalMode :: Mode -> PC -> IntState -> Op
evalMode Immediate n s = n
evalMode Position n s =
  let program = code s
  in  if n >= 0
        then program !! n
        else error
          (  "Cannot get value of positional parameter "
          ++ show n
          ++ " from "
          ++ show program
          )
evalMode Relative p s =
  let program = code s
      n       = p + relBase s
  in  if n >= 0
        then program !! n
        else error
          (  "Cannot get value of positional parameter "
          ++ show n
          ++ " from "
          ++ show program
          )

evalDestMode :: Mode -> PC -> IntState -> Op
evalDestMode Immediate n s = n
evalDestMode Position  n s = n
evalDestMode Relative  n s = n + relBase s

getParameters :: IntState -> ([Mode], [Op])
getParameters s = (modes, params)
 where
  i : ps     = intStateToPosition s
  params     = take maxOps ps
  (_, modes) = opToInstruction i

eval4OpFunc :: (Op -> Op -> Op) -> IntState -> IntState
eval4OpFunc f s = s { code = nextCode, pc = pc s + 4 }
 where
  (modes, params)      = getParameters s
  (p1 : p2 : dest : _) = params
  (m1 : m2 : m3   : _) = modes
  r2                   = evalMode m2 p2 s
  r1                   = evalMode m1 p1 s
  d                    = evalDestMode m3 dest s
  nextCode             = replaceOp d (f r1 r2) (code s)

evalOp1 :: IntState -> IntState
evalOp1 = eval4OpFunc (+)

evalOp2 :: IntState -> IntState
evalOp2 = eval4OpFunc (*)


evalOp3 :: IntState -> IntState
evalOp3 s = s { code = nextCode, pc = pc s + 2, input = tail (input s) }
 where
  (modes, params) = getParameters s
  m1              = head modes
  dest            = head params
  d               = evalDestMode m1 dest s
  nextCode        = replaceOp d (head (input s)) (code s)

evalOp4 :: IntState -> IntState
evalOp4 s = s { output = save : output s, pc = pc s + 2 }
 where
  (modes, params) = getParameters s
  save            = evalMode (head modes) (head params) s

evalOpJumpIfFunc :: (Op -> Op -> Bool) -> IntState -> IntState
evalOpJumpIfFunc f s = s { pc = newPC }
 where
  (modes, params) = getParameters s
  (m1 : m2 : _)   = modes
  (p1 : p2 : _)   = params
  (r1, r2)        = (evalMode m1 p1 s, evalMode m2 p2 s)
  newPC           = if r1 `f` 0 then r2 else pc s + 3

evalOp5 :: IntState -> IntState
evalOp5 = evalOpJumpIfFunc (/=)

evalOp6 :: IntState -> IntState
evalOp6 = evalOpJumpIfFunc (==)

evalOp7 :: IntState -> IntState
evalOp7 = eval4OpFunc (\r1 r2 -> if r1 < r2 then 1 else 0)

evalOp8 :: IntState -> IntState
evalOp8 = eval4OpFunc (\r1 r2 -> if r1 == r2 then 1 else 0)

evalOp9 :: IntState -> IntState
evalOp9 s = s { relBase = newRelBase, pc = pc s + 2 }
 where
  (modes, params) = getParameters s
  r1              = evalMode (head modes) (head params) s
  newRelBase      = relBase s + r1

runIntCode :: IntState -> IntState
runIntCode preState | op == 1                   = runIntCode $ evalOp1 s
                    | op == 2                   = runIntCode $ evalOp2 s
                    | op == 3 && null (input s) = s { status = Input }
                    | op == 3                   = runIntCode $ evalOp3 s
                    | op == 4                   = runIntCode $ evalOp4 s
                    | op == 5                   = runIntCode $ evalOp5 s
                    | op == 6                   = runIntCode $ evalOp6 s
                    | op == 7                   = runIntCode $ evalOp7 s
                    | op == 8                   = runIntCode $ evalOp8 s
                    | op == 9                   = runIntCode $ evalOp9 s
                    | op == 99                  = s { status = Halt }
                    | otherwise = error ("Unknown Opcode detected: " ++ show s)
 where
  s         = preState { code = code preState ++ repeat 0, status = Running }
  i@(op, _) = if pc s >= 0
    then opToInstruction (code s !! pc s)
    else error
      ("Cannot Read next op, PC:" ++ show (pc s) ++ " | " ++ show (code s))

evaluateCode :: [Op] -> [Op] -> IntState
evaluateCode intCode userInput =
  runIntCode (IntState 0 0 intCode userInput [] Running)
