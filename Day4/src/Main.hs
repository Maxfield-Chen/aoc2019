module Main where

import           Text.ParserCombinators.Parsec
import           Data.List
import           Data.Char                      ( digitToInt )
import           Debug.Trace

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

eval4OpFunc :: (Op -> Op -> Op) -> [Op] -> [Op] -> [Mode] -> ([Op], PC)
eval4OpFunc f code position@(_ : p1 : p2 : dest : _) (m1 : m2 : m3 : _) =
  (replaceOp dest (f r1 r2) code, 4)
 where
  r1 = evalMode m2 p2 code
  r2 = evalMode m1 p1 code

evalOp1 :: [Op] -> [Op] -> [Mode] -> ([Op], PC)
evalOp1 = eval4OpFunc (+)

evalOp2 :: [Op] -> [Op] -> [Mode] -> ([Op], PC)
evalOp2 = eval4OpFunc (*)

evalOp3 :: [Op] -> [Op] -> [Op] -> [Mode] -> ([Op], PC)
evalOp3 code position@(_ : p : _) input (m : _) =
  (replaceOp p (head input) code, 2)

evalOp4 :: [Op] -> [Op] -> [Op] -> [Mode] -> (Op, PC)
evalOp4 code position@(_ : p : _) input (m : _) = (evalMode m p code, 2)

-- Note this function assumes opcode 99 is handled externally
doOp :: PC -> [Op] -> [Op] -> ([Op], [Op]) -> (([Op], [Op]), ([Op], PC))
doOp pc code position (input, output)
  | op == 1
  = ((input, output), evalOp1 code position modes)
  | op == 2
  = ((input, output), evalOp2 code position modes)
  | op == 3
  = let newInput = case input of
          (_ : xs) -> xs
          _        -> input
    in  ((newInput, output), evalOp3 code position input modes)
  | op == 4
  = let (ret, pcInc) = evalOp4 code position input modes
    in  ((input, ret : output), (code, pcInc))
  | otherwise
  = error ("Unknown Opcode detected: " ++ show op)
 where
  i@(op, modes) = opToInstruction nextOp
  nextOp        = if pc < length code
    then code !! max 0 pc
    else error ("Cannot Read next op, PC:" ++ show pc ++ " | " ++ show code)

step :: PC -> [Op] -> ([Op], [Op]) -> ([Op], [Op])
step pc code (input, output)
  | op == 99  = (output, code)
  | otherwise = step (pc + pcInc) nextStep (nextInput, nextOutput)
 where
  op = if pc < length code
    then code !! max 0 pc
    else error ("Cannot Read next op, PC:" ++ show pc ++ " | " ++ show code)
  position = drop pc code
  ((nextInput, nextOutput), (nextStep, pcInc)) =
    doOp pc code position (input, output)

evaluateCode :: [Op] -> [Op] -> ([Op], [Op])
evaluateCode code userInput = step 0 code (userInput, [])

main :: IO ()
main = do
  input1 <- readFile
    "/home/nihliphobe/projects/haskell/aoc2019/Day4/data/part1.txt"
  --print (evaluateCode [04, 2, 99] [1])
  --print (evaluateCode [3, 0, 3, 1, 99] [1, 2, 3])
  --print (evaluateCode [4, 0, 4, 4, 99] [1, 2, 3])
  print (evaluateCode [1002, 0, 2, 3, 99] [1, 2, 3])
  --print (evaluateCode [3, 0, 2, 0, 2, 3, 99] [1, 2, 3, 4])
  --print (evaluateCode [3, 0, 2, 0, 2, 3, 4, 0, 99] [1, 2, 3, 4])
  --print (evaluateCode [101, 5, 2, 0, 3, 1, 4, 0, 99] [1, 2, 3, 4])
  case parseOps input1 of
    Left  err     -> fail (show err)
    Right program -> do
      print part1
      where part1 = evaluateCode program [1]
