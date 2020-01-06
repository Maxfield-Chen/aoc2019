module Main where

import           IntCode
import           Data.Ord
import           Data.List

main :: IO ()
main = do
  ampCode <- readFile fileName
  case parseOps ampCode of
    Left  err     -> fail (show err)
    Right program -> do
      print p1OptimalSettings
     where
      amps = replicate 5 (emptyIntState { code = program, status = Running })
      p1PhaseSettings   = permutations [0 .. 4]
      p2PhaseSettings   = permutations [5 .. 9]
      p1OptimalSettings = maximumBy (comparing fst)
        $ map (\ps -> (evalPhaseSetting amps ps, ps)) p1PhaseSettings

runAmpChain :: [IntState] -> [IntState]
runAmpChain amps =
  let start : as = amps
  in  foldl
        (\r@(lastAmp : _) amp ->
          runIntCode (amp { input = input amp ++ output lastAmp }) : r
        )
        [runIntCode start]
        as

evalPhaseSetting :: [IntState] -> [Int] -> Int
evalPhaseSetting amps phaseSettings = (head . output . head)
  (runAmpChain configuredAmps)
 where
  configuredAmps =
    let (a : as) = map (\(p, a) -> a { input = [p] }) (zip phaseSettings amps)
    in  a { input = input a ++ [0] } : as
