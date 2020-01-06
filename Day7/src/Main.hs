module Main where

import           IntCode
import           Data.Ord
import           Data.List

type Amp = [Op] -> IntState

main :: IO ()
main = do
  ampCode <- readFile fileName
  case parseOps ampCode of
    Left  err     -> fail (show err)
    Right program -> do
      print p1OptimalSettings
     where
      amps              = replicate 5 (evaluateCode program)
      p1PhaseSettings   = permutations [0 .. 4]
      p2PhaseSettings   = permutations [5 .. 9]
      p1OptimalSettings = maximumBy (comparing fst)
        $ map (\ps -> (evalPhaseSetting amps ps, ps)) p1PhaseSettings


evalPhaseSetting :: [Amp] -> [Int] -> Int
evalPhaseSetting amps phaseSettings = snd $ foldl
  (\(p : ps, sig) amp -> (ps, head (output (amp [p, sig]))))
  (phaseSettings, 0)
  amps

