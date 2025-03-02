module Main where

import GraphUtils (genGaltonWatson, genRandomTree, makePruferCode, makePruferGraph)
import GraphViz (visualizeDirectedUnlabelledGraph, visualizeUnlabelledGraph)
import SampleGraphs (createSampleGraphL8F8)
import System.Random (randomRIO)

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
  putStrLn "Genertating random tree..."
  randomTree <- genRandomTree 10
  visualizeDirectedUnlabelledGraph randomTree "out/Random.png"

  -- Test Galton-Watson
  putStrLn "Genertating Galton-Watson tree..."
  galtonWatson <- genGaltonWatson $ randomRIO (0, 2)
  visualizeDirectedUnlabelledGraph galtonWatson "out/GaltonWatson.png"

  putStrLn "Done"
