module Main where

import           IntCode
import           Data.Ord
import           Data.List
import           Debug.Trace

main :: IO ()
main = do
  ampCode <- readFile fileName
  case parseOps ampCode of
    Left  err     -> fail (show err)
    Right program -> do
      print optimalSettings
      print (evalLoopedPhaseSetting amps [9, 8, 7, 6, 5])
     where
      amps = replicate 5 (emptyIntState { code = program, status = Running })
      p1PhaseSettings = permutations [0 .. 4]
      p2PhaseSettings = permutations [5 .. 9]
      optimalSettings = maximumBy (comparing fst) $ map
        (\ps -> ((head . output . last) $ evalPhaseSetting amps ps, ps))
        p1PhaseSettings
      optimalLoopedSettings =
        maximum $ map (evalLoopedPhaseSetting amps) p2PhaseSettings

runAmpChain :: [IntState] -> [IntState]
runAmpChain amps =
  let start : as = amps
  in  reverse $ foldl
        (\r@(lastAmp : _) amp ->
          runIntCode (amp { input = input amp ++ output lastAmp }) : r
        )
        [runIntCode start]
        as

configureAmps :: [IntState] -> [Int] -> [IntState]
configureAmps amps phaseSettings =
  let (a : as) = map (\(p, a) -> a { input = [p] }) (zip phaseSettings amps)
  in  a { input = input a ++ [0] } : as

evalLoopedPhaseSetting :: [IntState] -> [Int] -> Int
evalLoopedPhaseSetting amps phaseSettings =
  runLoop $ configureAmps amps phaseSettings

runLoop :: [IntState] -> Int
runLoop amps | status lastAmp == Halt = head $ output lastAmp
             | otherwise              = runLoop $ runAmpChain nextAmps
 where
  lastAmp  = last amps
  a : as   = amps
  nextAmps = a { input = input a ++ output lastAmp } : as


evalPhaseSetting :: [IntState] -> [Int] -> [IntState]
evalPhaseSetting amps phaseSettings =
  runAmpChain $ configureAmps amps phaseSettings
