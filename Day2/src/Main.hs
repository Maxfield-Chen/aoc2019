module Main where

import           Text.ParserCombinators.Parsec
import           Data.List

type Op = Int
type PC = Int
type Noun = Int
type Verb = Int
type Output = Int
type Status = Bool

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

main :: IO ()
main = do
  input1 <- readFile
    "/home/nihliphobe/projects/haskell/aoc2019/Day2/data/part1.txt"
  case parseOps input1 of
    Left  err     -> fail (show err)
    Right program -> do
      print part1
      print part2
     where
      part1 = (head . evaluateCode) (programNV 12 2 program)
      part2 = case findValue 19690720 program of
        Nothing -> "Part 2 Errored.\n"
        Just (output, noun, verb) ->
          "Part 2: (" ++ show output ++ "," ++ show noun ++ "," ++ show verb ++ ")"


