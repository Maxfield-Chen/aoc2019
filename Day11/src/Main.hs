module Main where

import qualified IntCode                       as I
import           Data.Ord
import           Data.List
import qualified Data.Map                      as M
import           Graphics.Gloss
import           Debug.Trace

data Direction = North | East | South | West deriving (Show, Eq, Enum, Bounded)
data PaintColor = White | Black deriving (Show, Eq)

data PainterBot = PainterBot { direction :: Direction
                             , position :: (Int, Int)
                             , program :: I.IntState
                             , pathLog :: M.Map (Int, Int) PaintColor} deriving Show

pixelScale = 20

visualizePath :: M.Map (Int, Int) PaintColor -> Picture
visualizePath m = pictures $ M.foldrWithKey pathToVisual [] m
 where
  pathToVisual (x, y) pc ret =
    let outputColor | pc == White = white
                    | pc == Black = black
    in  translate (fromIntegral x * pixelScale)
                  (fromIntegral y * pixelScale)
                  (color outputColor (rectangleSolid pixelScale pixelScale))
          : ret

right :: Direction -> Direction
right = doTurn maxBound minBound succ

left :: Direction -> Direction
left = doTurn minBound maxBound pred

doTurn
  :: Direction
  -> Direction
  -> (Direction -> Direction)
  -> (Direction -> Direction)
doTurn end loop f d | d == end  = loop
                    | otherwise = f d

dirToIncrement :: Direction -> (Int, Int)
dirToIncrement d | d == North = (0, 1)
                 | d == East  = (1, 0)
                 | d == South = (0, -1)
                 | d == West  = (-1, 0)

fmap' :: (Int -> Int -> Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
fmap' f (x, y) (x', y') = (f x x', f y y')

colorAtPosition :: (Int, Int) -> M.Map (Int, Int) PaintColor -> PaintColor
colorAtPosition = M.findWithDefault Black

colorToInt :: PaintColor -> Int
colorToInt c | c == Black = 0
             | c == White = 1

intToColor :: Int -> PaintColor
intToColor i | i == 0    = Black
             | i == 1    = White
             | otherwise = error ("Unknown PaintColor: " ++ show i)

--Error occurs because robot moves in direction currently facing, not new direction
stepPainterBot :: PainterBot -> PainterBot
stepPainterBot bot | I.status (program bot) == I.Halt = bot
                   | otherwise                        = stepPainterBot nextBot
 where
  shipColor           = colorAtPosition (position bot) (pathLog bot)
  toRun               = (program bot) { I.input = [colorToInt shipColor] }
  nextProg            = I.runIntCode toRun
  lr : paintColor : _ = I.output nextProg
  (x, y)              = position bot
  nextLog = M.insert (position bot) (intToColor paintColor) (pathLog bot)
  nextPos             = fmap' (+) (position bot) (dirToIncrement nextDir)
  nextDir | lr == 0   = left (direction bot)
          | lr == 1   = right (direction bot)
          | otherwise = error "Bot attempted to turn in an invalid direction."
  nextBot = PainterBot nextDir nextPos nextProg nextLog

window :: Display
window = InWindow "PainterBot" (1000, 1000) (10, 10)

background :: Color
background = greyN 0.5

filename = "/home/nihliphobe/projects/haskell/aoc2019/Day11/data/input.txt"

main :: IO ()
main = do
  code <- readFile filename
  case I.parseOps code of
    Left  err     -> fail (show err)
    Right program -> do
      print $ pathLog spentBot
      print $ length (pathLog spentBot)
      display window background drawing
     where
      spentBot   = stepPainterBot (PainterBot North (0, 0) startState M.empty)
      startState = I.emptyIntState { I.code = program, I.status = I.Running }
      drawing    = visualizePath (pathLog spentBot)
