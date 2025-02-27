module GraphUtils where

import Data.Graph.Inductive
import Data.GraphViz
import Data.GraphViz.Commands
import System.FilePath (replaceExtension)

-- Common graph creation and manipulation functions
createSampleGraph :: Gr String String
createSampleGraph =
  mkGraph
    [(1, "Node 1"), (2, "Node 2"), (3, "Node 3")]
    [(1, 2, "Edge 1-2"), (2, 3, "Edge 2-3"), (3, 1, "Edge 3-1")]

-- Graph visualization function
visualizeGraph :: Gr String String -> FilePath -> IO FilePath
visualizeGraph g path = do
  let dotGraph = graphToDot quickParams g
  runGraphvizCommand Dot dotGraph Png path
