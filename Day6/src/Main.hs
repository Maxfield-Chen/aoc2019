module Main where


import           Text.ParserCombinators.Parsec
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Char                     as C
import qualified Data.Text                     as T

type Identifier = String
type ParentOrbits = M.Map Identifier [Identifier]
type Orbits = M.Map Identifier Identifier
type Hops = S.Set Identifier

fileName = "/home/nihliphobe/projects/haskell/aoc2019/Day6/data/part1.txt"
test1 = "/home/nihliphobe/projects/haskell/aoc2019/Day6/data/test1.txt"

pOrbits :: Parser [[Identifier]]
pOrbits = endBy pOrbit eol

pOrbit :: Parser [Identifier]
pOrbit = sepBy (many (noneOf ")\n")) (char ')')

eol = char '\n'

parseLinks :: String -> Either ParseError [[Identifier]]
parseLinks = parse pOrbits "(unknown)"

linksToOrbits :: [[Identifier]] -> Orbits
linksToOrbits = foldl applyLink M.empty

applyLink :: Orbits -> [Identifier] -> Orbits
applyLink ret (primary : sat : _) = M.insert sat primary ret

countOrbits :: Orbits -> Identifier -> Int
countOrbits o here | M.member here o = 1 + countOrbits o (o M.! here)
                   | otherwise       = 0

genOrbitalCheckSum :: Orbits -> Int
genOrbitalCheckSum o = sum $ map (countOrbits o) (M.keys o)

collectPath :: Orbits -> Hops -> Identifier -> Hops
collectPath o hops here | M.member here o = collectPath o hops' there
                        | otherwise       = hops
 where
  hops' = S.insert here hops
  there = o M.! here

hopsToSanta :: Orbits -> Int
hopsToSanta o = S.size onlyYou + S.size onlySan
 where
  youPath = collectPath o S.empty (o M.! "YOU")
  sanPath = collectPath o S.empty (o M.! "SAN")
  onlyYou = youPath S.\\ sanPath
  onlySan = sanPath S.\\ youPath

main :: IO ()
main = do
  input <- readFile fileName
  test1 <- readFile test1
  case parseLinks input of
    Left  err    -> fail (show err)
    Right orbits -> do
      print (genOrbitalCheckSum o)
      print (hopsToSanta o)
      where o = linksToOrbits orbits
