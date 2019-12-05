module Main where

import qualified Data.List                     as L

isMonotonic :: Int -> Bool
isMonotonic x = L.sort (show x) == show x

repeats :: Int -> Bool
repeats x = fst $ foldl hasDuplicate (False, 'a') (show x)
  where hasDuplicate (ret, old) new = (ret || (old == new), new)

get1 (r, _, _) = r

--repeatsTwice :: Int -> Bool
repeatsTwice x = (get1 . fst)
  (foldl hasDuplicate ((False, False, 'a'), 'a') (show x ++ ['a']))
 where
  hasDuplicate ((ret, potential, banned), old) new =
    ((ret || valid, newPotential, newBan), new)
   where
    repeating    = old == new
    inSeq        = new == banned
    valid        = potential && not repeating && not inSeq
    newBan       = if not repeating then 'a' else new
    newPotential = repeating && not inSeq
psSpace :: Int -> Int -> [Int]
psSpace s e = (filter isMonotonic . filter repeats) [s .. e]

main :: IO ()
main = do
  print (show p1)
  print (show p2)
 where
  p1 = length $ psSpace 347312 805915
  p2 = length (filter repeatsTwice (psSpace 347312 805915))
