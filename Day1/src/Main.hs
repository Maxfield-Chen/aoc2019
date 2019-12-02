module Main where

import           Text.ParserCombinators.Parsec

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

calcInitialFuel :: Module -> Fuel
calcInitialFuel mass = floor (dModuleMass / 3) - 2
  where dModuleMass = fromIntegral mass :: Double

calcFuel :: Module -> Fuel
calcFuel = incrementFuel 0
 where
  incrementFuel total increment
    | initialFuel <= 0 = total
    | otherwise        = incrementFuel (total + initialFuel) initialFuel
    where initialFuel = calcInitialFuel increment

main :: IO ()
main = do
  part1 <- readFile
    "/home/nihliphobe/projects/haskell/aoc2019/Day1/data/part1.txt"
  case parseModules part1 of
    Left  err     -> fail (show err)
    Right modules -> do
      putStrLn $ "Part 1: " ++ show part1
      putStrLn $ "Part 2: " ++ show part2
     where
      part1 = sum (map calcInitialFuel modules)
      part2 = sum (map calcFuel modules)

