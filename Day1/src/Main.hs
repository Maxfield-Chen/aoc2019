module Main where

import           Text.ParserCombinators.Parsec
import           Data.Maybe                    as Maybe
import qualified Data.Map                      as M

type Module = Int
type Fuel = Int

pModules :: Parser [Module]
pModules = endBy pModule eol

pModule :: Parser Module
pModule = do
  digits <- many1 digit
  return (read digits)

eol :: Parser Char
eol = char '\n'

parseModules :: String -> Either ParseError [Module]
parseModules = parse pModules "(unknown)"

calcFuel :: Module -> Fuel
calcFuel moduleMass = floor (dModuleMass / 3) - 2
  where dModuleMass = fromIntegral moduleMass :: Double

main :: IO ()
main = do
  part1 <- readFile "/home/nihliphobe/projects/haskell/aoc2019/Day1/data/part1.txt"
  case parseModules part1 of
    Left err -> fail (show err)
    Right modules -> print sumFuel
      where sumFuel = sum (map calcFuel modules)
