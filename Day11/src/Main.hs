module Main where

import qualified IntCode                       as I
import           Data.Ord
import           Data.List
import           Control.Lens

data Direction = North | East | South | West deriving (Show, Eq, Enum, Bounded)


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
dirToIncrement d | d == North = (0, -1)
                 | d == East  = (1, 0)
                 | d == South = (0, 1)
                 | d == West  = (-1, 0)

colorAtPosition :: PainterBot -> Int
colorAtPosition p = let (x, y) = position p in (ship p !! y) !! x

fmap' :: (Int -> Int -> Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
fmap' f (x, y) (x', y') = (f x x', f y y')

maximumSideLength = 5000

data PainterBot = PainterBot { ship :: [[Int]]
                             , direction :: Direction
                             , position :: (Int, Int)
                             , program :: I.IntState} deriving Show

stepPainterBot :: PainterBot -> PainterBot
stepPainterBot bot | I.status (program bot) == I.Halt = bot
                   | otherwise                        = stepPainterBot nextBot
 where
  shipColor           = colorAtPosition bot
  toRun               = (program bot) { I.input = [shipColor] }
  nextProg            = I.runIntCode toRun
  paintColor : lr : _ = I.output nextProg
  (x, y)              = position bot
  nextShip            = ship bot & element y . element x .~ paintColor
  nextPos = fmap' (+) (position bot) (dirToIncrement (direction bot))
  nextDir | lr == 0   = left (direction bot)
          | lr == 1   = right (direction bot)
          | otherwise = error "Bot attempted to turn in an invalid direction."
  nextBot = PainterBot nextShip nextDir nextPos nextProg

main :: IO ()
main = do
  code <- readFile I.fileName
  case I.parseOps code of
    Left  err     -> fail (show err)
    Right program -> print squaresPainted
     where
      squaresPainted =
        foldl (\a xs -> a + length xs) 0 $ fmap (filter odd) (ship spentBot)
      spentBot =
        stepPainterBot (PainterBot blackShip North (sPos, sPos) startState)
      startState = I.emptyIntState { I.code = program, I.status = I.Running }
      blackShip  = replicate maximumSideLength (replicate maximumSideLength 0)
      sPos       = maximumSideLength `div` 2
