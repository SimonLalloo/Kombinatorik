module Main where

import GraphUtils

main :: IO ()
main = do
  putStrLn "Running Section 1"
  let graph = createSampleGraph
  outputPath <- visualizeGraph graph "out/section1-graph.png"
  putStrLn $ "Graph visualization saved to: " ++ outputPath
