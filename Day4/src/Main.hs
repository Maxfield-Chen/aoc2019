module Main where

import           Text.ParserCombinators.Parsec
import           Data.List
import           Data.Char                      ( digitToInt )
import           Debug.Trace

data IntState = IntState { pc :: PC
                         , code :: [Op]
                         , input :: [Op]
                         , output :: [Op]}

type Op = Int
type PC = Int
type Noun = Int
type Verb = Int
type Output = Int
type Status = Bool
data Mode = Position | Immediate deriving (Show, Eq)
type Instruction = (Op, [Mode])

maxOps = 3

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
  mode | digitToInt ms == 1 = Immediate
       | otherwise          = Position

parseModes ret m = reverse ret

rpad :: Int -> Mode -> [Mode] -> [Mode]
rpad n mode ret | length ret < n = rpad n mode (ret ++ [mode])
                | otherwise      = ret

opToInstruction :: Op -> Instruction
opToInstruction i = (op, rpad (maxOps + 1) Position modes)
 where
  s     = show i
  rs    = reverse s
  op    = read (reverse (take 2 rs)) :: Int
  modes = parseModes [] (drop 2 rs)

replaceOp :: PC -> Op -> [Op] -> [Op]
replaceOp n x code = take n code ++ [x] ++ drop (n + 1) code

evalMode :: Mode -> PC -> [Op] -> Op
evalMode Immediate n code = n
evalMode Position  n code = if n < length code
  then code !! max 0 n
  else error
    (  "Cannot get value of positional parameter "
    ++ show n
    ++ " from "
    ++ show code
    )

eval4OpFunc :: (Op -> Op -> Op) -> IntState -> IntState
eval4OpFunc f s = s { code = nextCode, pc = pc s + 4 }
 where
  r2                       = evalMode m2 p2 (code s)
  r1                       = evalMode m1 p1 (code s)
  (i : p1 : p2 : dest : _) = drop (pc s) (code s)
  (m1          : m2   : _) = modes
  (nextOp, modes)          = opToInstruction i
  nextCode                 = replaceOp dest (f r1 r2) (code s)

evalOp1 :: IntState -> IntState
evalOp1 = eval4OpFunc (+)

evalOp2 :: IntState -> IntState
evalOp2 = eval4OpFunc (*)

evalOp3 :: IntState -> IntState
evalOp3 s = s { code = nextCode, pc = pc s + 2, input = tail (input s) }
 where
  nextCode     = replaceOp p1 (head (input s)) (code s)
  (_ : p1 : _) = drop (pc s) (code s)

evalOp4 :: IntState -> IntState
evalOp4 s = s { output = save : output s, pc = pc s + 2 }
 where
  (_, modes)   = opToInstruction i
  (i : p1 : _) = drop (pc s) (code s)
  save         = evalMode (head modes) p1 (code s)

-- Note this function assumes opcode 99 is handled externally
runIntCode :: IntState -> IntState
runIntCode s | op == 1   = runIntCode $ evalOp1 s
             | op == 2   = runIntCode $ evalOp2 s
             | op == 3   = runIntCode $ evalOp3 s
             | op == 4   = runIntCode $ evalOp4 s
             | op == 99  = s
             | otherwise = error ("Unknown Opcode detected: " ++ show op)
 where
  (op, _) = if pc s < length (code s)
    then opToInstruction (code s !! max 0 (pc s))
    else error
      ("Cannot Read next op, PC:" ++ show (pc s) ++ " | " ++ show (code s))

evaluateCode :: [Op] -> [Op] -> ([Op], [Op])
evaluateCode intCode userInput =
  let endState = runIntCode (IntState 0 intCode userInput [])
  in  (output endState, code endState)

main :: IO ()
main = do
  input1 <- readFile
    "/home/nihliphobe/projects/haskell/aoc2019/Day4/data/part1.txt"
  print (evaluateCode [04, 2, 99] [1])
  print (evaluateCode [3, 0, 3, 1, 99] [100, 101, 102])
  print (evaluateCode [4, 0, 4, 4, 99] [1, 2, 3])
  print (evaluateCode [104, 0, 4, 4, 99] [1, 2, 3])
  print (evaluateCode [1002, 0, 2, 3, 99] [1, 2, 3])
  print (evaluateCode [3, 0, 2, 0, 2, 3, 99] [1, 2, 3, 4])
  print (evaluateCode [3, 0, 2, 0, 2, 3, 4, 0, 99] [1, 2, 3, 4])
  print (evaluateCode [101, 5, 2, 0, 3, 1, 4, 0, 99] [99, 2, 3, 4])
  case parseOps input1 of
    Left  err     -> fail (show err)
    Right program -> do
      print part1
      print "Done"
      where part1 = evaluateCode program [1]
