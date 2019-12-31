module Main where

import           IntCode
import           Data.Ord
import           Data.List

type Amp = [Op] -> [Op]

main :: IO ()
main = do
  ampCode <- readFile fileName
  case parseOps ampCode of
    Left  err     -> fail (show err)
    Right program -> do
      print optimalSettings
     where
      amps            = replicate 5 (evaluateCode program)
      phaseSettings   = permutations [0, 1, 2, 3, 4]
      optimalSettings = maximumBy (comparing fst)
        $ map (\ps -> (evalPhaseSetting amps ps, ps)) phaseSettings


evalPhaseSetting :: [Amp] -> [Int] -> Int
evalPhaseSetting amps phaseSettings = snd $ foldl
  (\(p : ps, sig) amp -> (ps, head (amp [p, sig])))
  (phaseSettings, 0)
  amps

