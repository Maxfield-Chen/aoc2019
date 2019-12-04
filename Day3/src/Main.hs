module Main where

import           Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Function as F
import Data.Maybe (Maybe, fromMaybe)

data Direction = U | R | D | L deriving (Enum,Show)
type Distance = Int
type Vector = (Direction, Distance)
type Coord = (Int, Int)
type Step = Int
type Wire = M.Map Coord Step

pWires :: Parser [[Vector]]
pWires = endBy pVectors eol

pVectors :: Parser [Vector]
pVectors = sepBy pVector comma

-- Handle non-exhaustive case here (maybe?)
pVector :: Parser Vector
pVector = do
  pDirection <- oneOf "URDL"
  distance <- many1 digit
  let direction = case pDirection of 
                      'U' -> U
                      'R' -> R
                      'D' -> D
                      'L' -> L in return (direction, read distance)

eol :: Parser Char
eol = char '\n'

comma :: Parser Char
comma = char ','

parseWires :: String -> Either ParseError [[Vector]]
parseWires = parse pWires "(unknown)"

directionToCoord :: Direction -> Coord
directionToCoord direction = case direction of 
                               U -> (0,1)
                               R -> (1,0)
                               D -> (0,-1)
                               L -> (-1,0)

applyVector :: (Coord, Step, Wire) -> Vector -> (Coord, Step, Wire)
applyVector start (direction, distance) = (end,ss+distance, newWire)
  where 
        newWire = M.unionWith min wire (M.fromList paths)
        ((xs,ys),ss,wire) = start
        end = (xs + distance * xd, ys + distance*yd)
        paths = map (\d -> ((xs + d*xd, ys + d*yd), ss + d)) deltas
        deltas = [1..distance]
        (xd, yd) = directionToCoord direction

traceWire :: [Vector] -> Wire
traceWire vs = M.delete (0,0) wire 
  where (_,_,wire) = foldl applyVector ((0,0), 0, M.empty ) vs 

cabDistance :: Coord -> Coord -> Distance
cabDistance (p1, p2) (q1,q2) = abs (p1-q1) + abs (p2-q2)

main :: IO ()
main = do
  input <- readFile "/home/nihliphobe/projects/haskell/aoc2019/Day3/data/part1.txt"
  case parseWires input of
    Left err -> fail (show err)
    Right wires -> do
      print p1
      print p2
      where 
            p1 = L.minimumBy (compare `F.on` fst) cabs
            p2 = L.minimumBy (compare `F.on` snd) (M.toList jointSteps)
            cabs = map (\c -> (cabDistance (0,0) c, c)) (M.keys i)
            i =w `M.intersection` w'
            i' =w' `M.intersection` w
            jointSteps = M.mapWithKey (\k s -> s + fromMaybe 0 (M.lookup k i)) i'
            w = traceWire (head wires)
            w' = traceWire (last wires)
