module Main where

import           Text.ParserCombinators.Parsec
import           Data.List
import           Data.Char                      ( digitToInt )

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
pOps = sepBy pOp comma

pOp :: Parser Op
pOp = do
  digits <- many1 digit
  return (read digits)

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
 where
  mode | (digitToInt . head) m == 1 = Immediate
       | otherwise                  = Position

rpad :: Int -> Mode -> [Mode] -> [Mode]
rpad n mode ret | length ret < n = rpad n mode (ret ++ [mode])
                | otherwise      = ret


-- Takes a reversed op string as an argument, returns list of modes
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
evalMode Position  n code = code !! max 0 n

eval4OpFunc :: (Op -> Op -> Op) -> [Op] -> [Mode] -> ([Op], PC)
eval4OpFunc f code@(_ : p1 : p2 : dest : _) (m1 : m2 : m3 : _) =
  (replaceOp dest (f r1 r2) code, 4)
 where
  r1 = evalMode m2 p2 code
  r2 = evalMode m1 p1 code

evalOp1 :: [Op] -> [Mode] -> ([Op], PC)
evalOp1 = eval4OpFunc (+)

evalOp2 :: [Op] -> [Mode] -> ([Op], PC)
evalOp2 = eval4OpFunc (*)

evalOp3 :: [Op] -> [Op] -> [Mode] -> ([Op], PC)
evalOp3 code@(_ : p : _) input (m : _) = (replaceOp p (head input) code, 2)

evalOp4 :: [Op] -> [Op] -> [Mode] -> (Op, PC)
evalOp4 code@(_ : p : _) input (m : _) = (code !! p, 2)



-- Note this function assumes opcode 99 is handled externally
doOp :: PC -> [Op] -> ([Op], [Op]) -> (([Op], [Op]), ([Op], Int))
doOp n code (input, output)
  | op == 1
  = ((input, output), evalOp1 code modes)
  | op == 2
  = ((input, output), evalOp2 code modes)
  | op == 3
  = ((tail input, output), evalOp3 code input modes)
  | op == 4
  = let (ret, newPC) = evalOp4 code input modes
    in  ((input, ret : output), (code, newPC))
  | otherwise
  = error ("Unknown Opcode detected: " ++ show op)
 where
  i@(op, modes) = opToInstruction nextOp
  nextOp        = head code

-- TODO: Handle the invalid list case where head fails here.
evaluateCode :: [Op] -> [Op] -> ([Op], [Op])
evaluateCode code userInput = step 0 code (userInput, [])
 where
  step pc code (input, output)
    | op == 99  = (output, code)
    | otherwise = step (pc + pcInc) nextStep (nextInput, nextOutput)
   where
    op = code !! max 0 pc
    ((nextInput, nextOutput), (nextStep, pcInc)) = doOp pc code (input, output)

main :: IO ()
main = do
  input1 <- readFile
    "/home/nihliphobe/projects/haskell/aoc2019/Day4/data/part1.txt"
  case parseOps input1 of
    Left  err     -> fail (show err)
    Right program -> do
      print part1
      where part1 = fst (evaluateCode program [1])
