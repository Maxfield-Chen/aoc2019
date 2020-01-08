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
      print result
     where
      test = I.runIntCode $ I.emptyIntState
        { I.code  = [109, 1, 203, 0, 04, 1, 99]
        , I.input = [999]
        }
      result =
        I.runIntCode $ I.emptyIntState { I.code = program, I.input = [1] }
