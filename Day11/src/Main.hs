module Main where

import qualified IntCode                       as I
import           Data.Ord
import           Data.List
import qualified Data.Map.Strict               as M

data Direction = North | East | South | West deriving (Show, Eq, Enum, Bounded)
data Color = White | Black deriving (Show, Eq)


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

data PainterBot = PainterBot { direction :: Direction
                             , position :: (Int, Int)
                             , program :: I.IntState
                             , pathLog :: M.Map (Int, Int) Color} deriving Show

colorAtPosition :: (Int, Int) -> M.Map (Int, Int) Color -> Color
colorAtPosition = M.findWithDefault Black

colorToInt :: Color -> Int
colorToInt c | c == Black = 0
             | c == White = 1

intToColor :: Int -> Color
intToColor i | i == 0    = Black
             | i == 1    = White
             | otherwise = error ("Unknown Color: " ++ show i)


stepPainterBot :: PainterBot -> PainterBot
stepPainterBot bot | I.status (program bot) == I.Halt = bot
                   | otherwise                        = stepPainterBot nextBot
 where
  shipColor           = colorAtPosition (position bot) (pathLog bot)
  toRun               = (program bot) { I.input = [colorToInt shipColor] }
  nextProg            = I.runIntCode toRun
  paintColor : lr : _ = I.output nextProg
  (x, y)              = position bot
  nextLog = M.insert (position bot) (intToColor paintColor) (pathLog bot)
  nextPos = fmap' (+) (position bot) (dirToIncrement (direction bot))
  nextDir | lr == 0   = left (direction bot)
          | lr == 1   = right (direction bot)
          | otherwise = error "Bot attempted to turn in an invalid direction."
  nextBot = PainterBot nextDir nextPos nextProg nextLog

main :: IO ()
main = do
  code <- readFile I.fileName
  case I.parseOps code of
    Left  err     -> fail (show err)
    Right program -> do
      print $ pathLog spentBot
      print $ length (pathLog spentBot)
     where
      spentBot   = stepPainterBot (PainterBot North (0, 0) startState M.empty)
      startState = I.emptyIntState { I.code = program, I.status = I.Running }
