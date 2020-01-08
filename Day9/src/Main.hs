module Main where

import qualified IntCode                       as I
import           Data.Ord
import           Data.List

main :: IO ()
main = do
  code <- readFile I.fileName
  case I.parseOps code of
    Left  err     -> fail (show err)
    Right program -> do
      print $ I.output testMode
      print $ I.output boostMode
     where
      testMode =
        I.runIntCode $ I.emptyIntState { I.code = program, I.input = [1] }
      boostMode =
        I.runIntCode $ I.emptyIntState { I.code = program, I.input = [2] }
