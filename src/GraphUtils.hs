{-# LANGUAGE TupleSections #-}

module GraphUtils (
  makePruferCode,
  makePruferGraph,
  genRandomTree,
  genGaltonWatson,
  randomFromTree,
  degrees,
  avgDegree,
  maxDegree,
  minDegree,
  diameter,
  poisson,
  avg,
  caroWei,
  greedyUnconnected,
  genErdosRenyi,
)
where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.Graph.Inductive (
  Gr,
  Graph (noNodes),
  deg,
  delNode,
  labEdges,
  neighbors,
  nodes,
 )
import Data.Graph.Inductive.Graph
import Data.List (maximumBy, sort, sortBy, (\\))
import Data.Ord (comparing)
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import Text.Printf (printf)

----------------------------------------
--------------- General ----------------
----------------------------------------

findMinLeaf :: Gr () () -> Int
findMinLeaf g = minimum $ filter (\n -> deg g n == 1) $ nodes g

-- Find the furthest node from i, and the distance to it
findFurthest :: Gr () () -> Node -> (Node, Int)
findFurthest g i
  | length (nodes g) == 1 = (1, 0)
  | otherwise = maximumBy (comparing snd) $ map (\neighbor -> findFurthest' g neighbor i 1) (neighbors g i)

findFurthest' :: Gr () () -> Node -> Node -> Int -> (Node, Int)
findFurthest' g i prev n
  | length (neighbors g i) <= 1 =
      (i, n)
  | otherwise =
      maximumBy (comparing snd) $ map (\neighbor -> findFurthest' g neighbor i (n + 1)) (neighbors g i \\ [prev])

----------------------------------------
--------------- Prufer -----------------
----------------------------------------

makePruferCode :: Gr () () -> [Int]
makePruferCode g | noNodes g <= 2 = []
makePruferCode g =
  let minLeaf = findMinLeaf g
      neighbor = head (neighbors g minLeaf)
   in neighbor : makePruferCode (delNode minLeaf g)

-- This makes a directed graph so that we can have a rooted tree
-- which is necessary for some of the other tasks
makePruferGraph :: [Int] -> Gr () ()
makePruferGraph code =
  let n = length code + 2
      vertices = [1 .. n]
      edges = makeEdgesFromCode code vertices
      labeledEdges = map (\(from, to) -> (from, to, ())) edges
   in mkGraph (map (,()) vertices) labeledEdges

-- This is missing error handlig and a bunch of cases, but
-- it should be fine when it's called by makePruferGraph.
makeEdgesFromCode :: [Int] -> [Int] -> [(Int, Int)]
makeEdgesFromCode _ [a, b] = [(a, b)]
makeEdgesFromCode (x : xs) vertices =
  let l = minimum $ vertices \\ (x : xs)
      edge = (x, l)
   in edge : makeEdgesFromCode xs (filter (/= l) vertices)

-- This is the initial version of makePruferGraph, which makes the graph undirected.
-- The only difference is the use of mkUGraph and labels being simpler
{-
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
-}

----------------------------------------
------------ Galton-Watson -------------
----------------------------------------

genGaltonWatson :: IO Int -> IO (Gr () ())
genGaltonWatson r = do
  genGaltonWatson' r [1] [(1, ())] []

-- Helper function to generate children
genGaltonWatson' :: IO Int -> [Int] -> [LNode ()] -> [LEdge ()] -> IO (Gr () ())
genGaltonWatson' _ [] v e = return $ mkGraph v e
genGaltonWatson' r (x : xs) v e = do
  childCount <- r
  let (unhandled, v2, e2) = genGaltonWatson'' childCount x (xs, v, e)
  genGaltonWatson' r unhandled v2 e2

-- Helper function to add children
genGaltonWatson'' :: Int -> Int -> ([Int], [LNode ()], [LEdge ()]) -> ([Int], [LNode ()], [LEdge ()])
genGaltonWatson'' 0 _ (unhandled, v, e) = (unhandled, v, e)
genGaltonWatson'' n node (unhandled, v, e) =
  let child = 1 + length v
   in genGaltonWatson'' (n - 1) node (child : unhandled, (child, ()) : v, (node, child, ()) : e)

----------------------------------------
-------------- Randomness --------------
----------------------------------------

-- Generates a random Prufer Code
genRandomCode :: Int -> IO [Int]
genRandomCode n = replicateM n $ randomRIO (1, n)

-- Generates a random tree with n nodes (requires n > 2)
genRandomTree :: Int -> IO (Gr () ())
genRandomTree 1 = return $ mkGraph [(1, ())] []
genRandomTree 2 = return $ mkGraph [(1, ()), (2, ())] [(1, 2, ())]
genRandomTree n = do
  code <- genRandomCode (n - 2)
  return $ makePruferGraph code

-- Generates a random number from a tree as described in task 5.
randomFromTree :: Gr () () -> IO Int
randomFromTree g = do
  r <- randomRIO (1, noNodes g)
  return $ outdeg g r

-- Generate a random number from the poisson distribution
poisson :: Double -> IO Int
poisson lambda = do
  let l = exp (-lambda)
      k = 0
      p = 1.0
  poisson' k p l

poisson' :: Int -> Double -> Double -> IO Int
poisson' k p l = do
  let k' = k + 1
  u <- randomRIO (0.0, 1.0)
  let p' = p * u
  if p' > l
    then
      poisson' k' p' l
    else
      return $ k' - 1

----------------------------------------
----------------- Stats ----------------
----------------------------------------

degrees :: Gr () () -> [Int]
degrees g = map (deg g) $ nodes g

avgDegree :: Gr () () -> Double
avgDegree g = avg $ degrees g

avg :: (Real a, Fractional b) => [a] -> b
avg xs = realToFrac (sum xs) / fromIntegral (length xs)

maxDegree :: Gr () () -> Int
maxDegree g = maximum $ degrees g

minDegree :: Gr () () -> Int
minDegree g = minimum $ degrees g

diameter :: Gr () () -> Int
diameter g
  | length (nodes g) <= 1 = 0
  | otherwise =
      let (node, _) = findFurthest g 1
          (_, distance) = findFurthest g node
       in distance

----------------------------------------
--------------- Caro-Wei ---------------
----------------------------------------

caroWei :: Gr () () -> IO [Int]
caroWei g = do
  labeledGraph <- addRandomLabels g
  return $ caroWei' g $ noNodes g

caroWei' :: Gr () () -> Int -> [Int]
caroWei' g 0 = []
caroWei' g n =
  let l = map (\i -> lab g n < lab g i) $ neighbors g n
   in if and l then n : caroWei' g (n - 1) else caroWei' g (n - 1)

-- This uses the variant with random labels described in task 7
greedyUnconnected :: Gr () () -> IO [Int]
greedyUnconnected g = do
  labeledGraph <- addRandomLabels g
  let sortedNodes = sortBy (on compare snd) (labNodes labeledGraph)
  return $ greedyUnconnected' labeledGraph sortedNodes []

greedyUnconnected' :: Gr Int () -> [(Int, Int)] -> [Int] -> [Int]
greedyUnconnected' g [] set = set
greedyUnconnected' g ((node, _) : xs) set =
  let l = map (`elem` set) $ neighbors g node
   in if or l then greedyUnconnected' g xs set else greedyUnconnected' g xs (node : set)

addRandomLabels :: Gr () () -> IO (Gr Int ())
addRandomLabels g = do
  labels <- shuffleM [1 .. noNodes g]
  return $ gmap (\(ins, n, _, outs) -> (ins, n, labels !! (n - 1), outs)) g

----------------------------------------
-------------- Erdös-Renyi -------------
----------------------------------------

genErdosRenyi :: Int -> Double -> IO (Gr () ())
genErdosRenyi n p = do
  let possibleEdges = [(i, j) | i <- [1 .. n], j <- [1 .. n], i < j]
  edges <- genEdges possibleEdges p
  return $ mkUGraph [1 .. n] edges

genEdges :: [(Int, Int)] -> Double -> IO [(Int, Int)]
genEdges [] _ = return []
genEdges (x : xs) p = do
  r <- randomRIO (0.0, 1.0)
  recursion <- genEdges xs p
  return $ if r < p then x : recursion else recursion
