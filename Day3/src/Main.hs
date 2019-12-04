module Main where

import           Text.ParserCombinators.Parsec
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Function as F

data Direction = U | R | D | L deriving (Enum,Show)
type Distance = Int
type Vector = (Direction, Distance)
type StepCoord = (Int, Int, Int)
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

applyVector :: (StepCoord, S.Set StepCoord) -> Vector -> (StepCoord, S.Set StepCoord)
applyVector start (direction, distance) = (end, wire `S.union` S.fromList paths)
  where 
        ((ss,xs,ys),wire) = start
        end = (ss+distance, xs + distance * xd, ys + distance*yd)
        paths = map (\d -> (ss + d, xs + d*xd, ys + d*yd)) deltas
        deltas = [1..distance]
        (xd, yd) = directionToCoord direction

scToC :: S.Set StepCoord -> S.Set Coord
scToC = S.map (\(_,x,y) -> (x,y))

vecToSC :: [Vector] -> S.Set StepCoord
vecToSC vs =   ret `S.difference` S.singleton (0,0,0)
  where (_,ret) = foldl applyVector ((0,0,0), S.singleton (0,0,0)) vs 

cabDistance :: Coord -> Coord -> Distance
cabDistance (p1, p2) (q1,q2) = abs (p1-q1) + abs (p2-q2)

combineSteps :: S.Set StepCoord -> S.Set StepCoord -> (S.Set StepCoord,S.Set StepCoord)
combineSteps w w' = (r,r')
  where 
    r = S.map (\sc@(s,x,y) -> (s + findMinMatch sc w',x,y)) w
    r'= S.map (\sc@(s,x,y) -> (s + findMinMatch sc w,x,y)) w'

findMinMatch :: StepCoord -> S.Set StepCoord -> Int
findMinMatch (_,x,y) w = sel1 (getMin matches)
  where 
    matches = S.filter (\(_,x',y') -> x == x' && y == y') w  
    getMin sc 
      | sc == S.empty = (0,0,0)
      | otherwise = S.findMin sc

sel1 :: (a, b, c) -> a
sel1 (r,_,_) = r
sel2 :: (a, b, c) -> b
sel2 (_,r,_) = r
sel3 :: (a, b, c) -> c
sel3 (_,_,r) = r

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
            p2 = L.minimumBy (compare `F.on` sel1) stepCrosses
            cabs = map (\c -> (cabDistance (0,0) c, c)) (S.toList crosses)
            crosses =scToC w1 `S.intersection` scToC w2
            stepCrosses = sw `S.intersection` sw'
            w1 = vecToSC (head wires)
            w2 = vecToSC (last wires)
            (sw, sw') = combineSteps w1 w2
