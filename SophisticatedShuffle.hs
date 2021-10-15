{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import System.Random

-- A complete binary tree, of leaves and internal nodes
-- Internal node: Node card l r
-- Where card is the number of leaves under the node
-- Invairant: card >= 2. All internal tree nodes are alwayes full.

data Tree a = Leaf a | Node !Int (Tree a) (Tree a) deriving (Show)

-- Convert a non-empty sequence (e1...eN) to a complete binary tree
buildTree = growLevel . (map Leaf)
  where
    growLevel [node] = node
    growLevel l = growLevel $ inner l

    inner [] = []
    inner x@[_] = x
    inner (e1 : e2 : rest) = (join e1 e2) : inner rest

    join l@(Leaf _) r@(Leaf _) = Node 2 l r
    join l@(Node ct _ _) r@(Leaf _) = Node (ct + 1) l r
    join l@(Leaf _) r@(Node ct _ _) = Node (ct + 1) l r
    join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl + ctr) l r

shuffle :: [a] -> [Int] -> [a]
shuffle elem rseq = shuffle' (buildTree elem) rseq
  where
    shuffle' (Leaf e) [] = [e]
    shuffle' tree (ri : r_others) =
      extractTree
        ri
        tree
        (\tree -> shuffle' tree r_others)

    -- extractTree n tree
    -- extracts the n-th element from the tree and returns
    -- that element, paired with a tree with the element deleted
    -- (only instead of pairings, we use CPS). The function maintains the
    -- invariant of completeness of the tree: all internals nodes are always full.
    -- The collection of patterns below is deliberately not complete.
    -- All the missing cases may not occur (anf if they do that's an error)

    extractTree 0 (Node _ (Leaf e) r) k = e : k r
    extractTree 1 (Node 2 l@Leaf {} (Leaf r)) k = r : k l
    extractTree n (Node c l@Leaf {} r) k =
      extractTree (n -1) r (\new_r -> k $ Node (c -1) l new_r)
    extractTree n (Node n1 l (Leaf e)) k | n + 1 == n1 = e : k l
    extractTree n (Node c l@(Node cl _ _) r) k
      | n < cl = extractTree n l (\new_l -> k $ Node (c -1) new_l r)
      | otherwise = extractTree (n - cl) r (\new_r -> k $ Node (c -1) l new_r)

makeRNG :: RandomGen g => Int -> g -> ([Int], g)
makeRNG n g = loop [] n g
  where
    loop acc 0 g = (reverse acc, g)
    loop acc n g =
      let (r, g') = randomR (0, n) g
       in loop (r : acc) (pred n) g'

main =
  let n = 1000000
   in print $ length $ shuffle [1 .. n + 1] (fst $ makeRNG n (mkStdGen 17))