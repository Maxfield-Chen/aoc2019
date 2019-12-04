module Main where

import           Text.ParserCombinators.Parsec
import qualified Data.Sequence as S
import qualified Data.List as L
import qualified Data.Function as F

data Direction = U | R | D | L deriving (Enum,Show)
type Distance = Int
type Vector = (Direction, Distance)
type Coord = (Int, Int)

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

applyVector :: [Coord] -> Vector -> [Coord]
applyVector wire (direction, distance) = wire ++ paths
  where 
        (xs,ys) = last wire
        paths = map (\(x,y) -> (xs + x*xd, ys + y*yd)) deltas
        deltas = zip [1..distance] [1..distance]
        (xd, yd) = directionToCoord direction

vecToCoords :: [Vector] -> [Coord]
vecToCoords = drop 1 . foldl applyVector [(0,0)]

cabDistance :: Coord -> Coord -> Distance
cabDistance (p1, p2) (q1,q2) = abs (p1-q1) + abs (p2-q2)

main :: IO ()
main = do
  input <- readFile "/home/nihliphobe/projects/haskell/aoc2019/Day3/data/part1.txt"
  case parseWires input of
    Left err -> fail (show err)
    Right wires -> 
      print p1
      where 
            p1 = L.minimumBy (compare `F.on` fst) cabs
            cabs = map (\c -> (cabDistance (0,0) c, c)) crosses
            crosses = w1 `L.intersect` w2
            w1 = vecToCoords ( head wires)
            w2 = vecToCoords (last wires)
