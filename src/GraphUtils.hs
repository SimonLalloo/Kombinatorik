module GraphUtils (makePruferCode, makePruferGraph, genRandomTree) where

import Control.Monad (replicateM)
import Data.Graph.Inductive (
  Gr,
  Graph (noNodes),
  deg,
  delNode,
  labEdges,
  mkUGraph,
  neighbors,
  nodes,
 )
import Data.List (sort, (\\))
import System.Random (randomRIO)
import Text.Printf (printf)

----------------------------------------
--------------- General ----------------
----------------------------------------

findMinLeaf :: Gr () () -> Int
findMinLeaf g = minimum $ filter (\n -> deg g n == 1) $ nodes g

----------------------------------------
--------------- Prufer -----------------
----------------------------------------

makePruferCode :: Gr () () -> [Int]
makePruferCode g | noNodes g <= 2 = []
makePruferCode g =
  let minLeaf = findMinLeaf g
      neighbor = head (neighbors g minLeaf)
   in neighbor : makePruferCode (delNode minLeaf g)

makePruferGraph :: [Int] -> Gr () ()
makePruferGraph code =
  let n = length code + 2
      vertices = [1 .. n]
      edges = makeEdgesFromCode code vertices
   in mkUGraph vertices edges

-- This is missing error handlig and a bunch of cases, but
-- it should be fine when it's called by makePruferGraph.
makeEdgesFromCode :: [Int] -> [Int] -> [(Int, Int)]
makeEdgesFromCode _ [a, b] = [(a, b)]
makeEdgesFromCode (x : xs) vertices =
  let l = minimum $ vertices \\ (x : xs)
      edge = if l < x then (l, x) else (x, l) -- because mkUGraph is stupid
   in edge : makeEdgesFromCode xs (filter (/= l) vertices)

genRandomCode :: Int -> IO [Int]
genRandomCode n = replicateM n $ randomRIO (1, n)

-- Generates a random tree with n nodes (requires n > 2)
genRandomTree :: Int -> IO (Gr () ())
genRandomTree n = do
  code <- genRandomCode (n - 2)
  return $ makePruferGraph code
