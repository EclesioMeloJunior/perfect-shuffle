module Main where

import System.Random

extract :: Int -> [a] -> (a, [a])
-- given a non-empty list l, extract the j-th element and return the element and
-- the remaining list. We wont worry about the order of the list of the remaining elements.
-- j >= 0, j == 0 corresponds to the first element.

extract 0 (h : t) = (h, t)
extract j l = loop j l []
  where
    loop 0 (h : t) acc = (h, acc ++ t)
    loop j (h : t) acc = loop (j -1) t (h : acc)

-- given a sequence (e1, ....en), n > 0, to shuffle, and a sequence (r1,...r[n-1]) of numbers such that
-- r[i] is an independent sample from a uniform random distribution [0 ... n-1], compute the corresponding
-- permutation of the input sequence.

shuffle :: [b] -> [Int] -> [b]

suffle [] [] = []

shuffle [e] [] = [e]
shuffle elements (r : r_others) =
  let (b, rest) = extract r elements
   in b : shuffle rest r_others

makeRNG :: RandomGen g => Int -> g -> ([Int], g)
makeRNG n g = loop [] n g
  where
    loop acc 0 g = (reverse acc, g)
    loop acc n g =
      let (r, g') = randomR (0, n) g
       in loop (r : acc) (pred n) g'

main =
  let n = 100000
   in print $ length $ shuffle [1 .. n + 1] (fst $ makeRNG n (mkStdGen 17))