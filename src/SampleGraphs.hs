module SampleGraphs (createSampleGraph1, createSampleGraphL8F8) where

import Data.Graph.Inductive (Gr, Graph (mkGraph))
import Data.Graph.Inductive.Graph (mkUGraph)

createSampleGraph1 :: Gr () ()
createSampleGraph1 =
  mkUGraph
    [1, 2, 3]
    [(1, 2), (1, 3), (2, 3)]

-- This is the sample graph from figure 8 in lecture 8
-- It has Prufer code 33773
createSampleGraphL8F8 :: Gr () ()
createSampleGraphL8F8 =
  mkUGraph
    [1, 2, 3, 4, 5, 6, 7]
    [(1, 3), (2, 3), (3, 6), (3, 7), (4, 7), (5, 7)]
