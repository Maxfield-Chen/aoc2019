module Main where

import           Text.ParserCombinators.Parsec
import           Data.List
import           Data.Char                      ( digitToInt )

data IntState = IntState { pc :: PC
                         , code :: [Op]
                         , input :: [Op]
                         , output :: [Op]}

type Op = Int
type PC = Int
data Mode = Position | Immediate deriving (Show, Eq)

maxOps = 3
fileName = "/home/nihliphobe/projects/haskell/aoc2019/Day5/data/part1.txt"

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
  (i : p1 : p2 : dest : _) = intStateToPosition s
  (nextOp, modes)          = opToInstruction i
  (m1 : m2 : _)            = modes
  r2                       = evalMode m2 p2 (code s)
  r1                       = evalMode m1 p1 (code s)
  nextCode                 = replaceOp dest (f r1 r2) (code s)

evalOp1 :: IntState -> IntState
evalOp1 = eval4OpFunc (+)

evalOp2 :: IntState -> IntState
evalOp2 = eval4OpFunc (*)

evalOp3 :: IntState -> IntState
evalOp3 s = s { code = nextCode, pc = pc s + 2, input = tail (input s) }
 where
  (_ : p1 : _) = intStateToPosition s
  nextCode     = replaceOp p1 (head (input s)) (code s)

evalOp4 :: IntState -> IntState
evalOp4 s = s { output = save : output s, pc = pc s + 2 }
 where
  (i : p1 : _) = intStateToPosition s
  (_, modes)   = opToInstruction i
  save         = evalMode (head modes) p1 (code s)

evalOpJumpIfFunc :: (Op -> Op -> Bool) -> IntState -> IntState
evalOpJumpIfFunc f s = s { pc = newPC }
 where
  (i : p1 : p2 : _) = intStateToPosition s
  (_, modes)        = opToInstruction i
  (m1 : m2 : _)     = modes
  (r1, r2)          = (evalMode m1 p1 (code s), evalMode m2 p2 (code s))
  newPC             = if r1 `f` 0 then r2 else pc s + 3

evalOp5 :: IntState -> IntState
evalOp5 = evalOpJumpIfFunc (/=)

evalOp6 :: IntState -> IntState
evalOp6 = evalOpJumpIfFunc (==)

evalOp7 :: IntState -> IntState
evalOp7 = eval4OpFunc (\r1 r2 -> if r1 < r2 then 1 else 0)

evalOp8 :: IntState -> IntState
evalOp8 = eval4OpFunc (\r1 r2 -> if r1 == r2 then 1 else 0)

runIntCode :: IntState -> IntState
runIntCode s | op == 1   = runIntCode $ evalOp1 s
             | op == 2   = runIntCode $ evalOp2 s
             | op == 3   = runIntCode $ evalOp3 s
             | op == 4   = runIntCode $ evalOp4 s
             | op == 5   = runIntCode $ evalOp5 s
             | op == 6   = runIntCode $ evalOp6 s
             | op == 7   = runIntCode $ evalOp7 s
             | op == 8   = runIntCode $ evalOp8 s
             | op == 99  = s
             | otherwise = error ("Unknown Opcode detected: " ++ show i)
 where
  i@(op, _) = if pc s < length (code s)
    then opToInstruction (code s !! max 0 (pc s))
    else error
      ("Cannot Read next op, PC:" ++ show (pc s) ++ " | " ++ show (code s))

evaluateCode :: [Op] -> [Op] -> [Op]
evaluateCode intCode userInput =
  let endState = runIntCode (IntState 0 intCode userInput [])
  in  output endState

main :: IO ()
main = do
  input1 <- readFile fileName
  case parseOps input1 of
    Left  err     -> fail (show err)
    Right program -> do
      print part1
      print part2
     where
      part1 = evaluateCode program [1]
      part2 = evaluateCode program [5]
