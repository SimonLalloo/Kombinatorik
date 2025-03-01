module Main where

import GraphUtils (makePruferCode, makePruferGraph, genRandomTree )
import GraphViz (visualizeUnlabelledGraph)
import SampleGraphs (createSampleGraphL8F8)

-- This requires that you have a directory "out/" in the same path as the executable
main :: IO ()
main = do
  -- Test the Prufer code conversion
  let graph = createSampleGraphL8F8
  outputPath <- visualizeUnlabelledGraph graph "out/F8L8-graph.png"
  let code = makePruferCode graph
  let newGraph = makePruferGraph code
  outputPath2 <- visualizeUnlabelledGraph newGraph "out/Prufer-graph.png"
  putStrLn $
    "Created graph at "
      ++ outputPath
      ++ " and generated its Prufer code, which is "
      ++ show code
      ++ " and recreated the graph from the code at "
      ++ outputPath2

  -- Test the random tree generation
  randomTree <- genRandomTree 10
  visualizeUnlabelledGraph randomTree "out/Random.png"

  putStrLn "Done"
