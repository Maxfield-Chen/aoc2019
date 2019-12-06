module Intcode where

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
opToModes :: Op -> Instruction
opToModes i = (op, rpad maxOps Position modes)
 where
  s     = show i
  rs    = reverse s
  op    = read (reverse (take 2 rs)) :: Int
  modes = parseModes [] (drop 2 rs)

-- Note this function assumes opcode 99 is handled externally
doOp :: PC -> [Op] -> (Status, [Op])
doOp n code
  | op == 1   = (True, take dest code ++ [a + b] ++ drop (dest + 1) code)
  | op == 2   = (True, take dest code ++ [a * b] ++ drop (dest + 1) code)
  | otherwise = error ("Unknown Opcode detected: " ++ show op)
 where
  (op : p1 : p2 : xs) = drop n code
  a                   = (last . take (p1 + 1)) code
  b                   = (last . take (p2 + 1)) code
  dest                = head xs

-- TODO: Handle the invalid list case where head fails here.
evaluateCode :: [Op] -> [Op]
evaluateCode = step 0
 where
  step pc code | op == 99  = code
               | status    = step (pc + 4) nextStep
               | otherwise = nextStep
   where
    op                 = head (drop pc code)
    (status, nextStep) = doOp pc code

programNV :: Noun -> Verb -> [Op] -> [Op]
programNV n v code = start ++ [n, v] ++ end
 where
  start = [head code]
  end   = drop 3 code

getOutput :: Noun -> Verb -> [Op] -> Output
getOutput noun verb program =
  (head . evaluateCode) (programNV noun verb program)

findValue :: Output -> [Op] -> Maybe (Output, Noun, Verb)
findValue desired program = Data.List.find isDesired outputs
 where
  isDesired (output, _, _) = output == desired
  outputs = map (\(n, v) -> (getOutput n v program, n, v)) combos
  combos  = [ (x, y) | x <- [0 .. 99], y <- [0 .. 99] ]

