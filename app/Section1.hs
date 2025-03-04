module Main where

import GraphUtils
import SampleGraphs (createSampleGraphL8F8)
import System.Random (randomRIO)
import Tasks (task10, task6)
import Visualization (makeHistogram, visualizeDirectedUnlabelledGraph, visualizeUnlabelledGraph)

-- TODO: Rename this module

-- This requires that you have a directory "out/" in the same path as the executable
main :: IO ()
main = do
  -- Test the Prufer code conversion
  let graph = createSampleGraphL8F8
  visualizeUnlabelledGraph graph "out/F8L8-graph.png"
  let code = makePruferCode graph
  putStrLn $ "The code is " ++ show code
  let newGraph = makePruferGraph code
  visualizeDirectedUnlabelledGraph newGraph "out/Prufer-graph.png"

  -- Test the random tree generation
  putStrLn "Genertating random tree with 10 nodes ..."
  randomTree <- genRandomTree 10
  visualizeDirectedUnlabelledGraph randomTree "out/Random.png"

  -- Test Galton-Watson
  putStrLn "Genertating Galton-Watson tree from that tree..."
  galtonWatson <- genGaltonWatson $ randomFromTree randomTree
  visualizeDirectedUnlabelledGraph galtonWatson "out/GaltonWatson.png"

  task6 1000

  erdosRenyi <- genErdosRenyi 10 0.5
  visualizeUnlabelledGraph erdosRenyi "out/ErdosRenyi.png"

  task10 10

  putStrLn "Done"
