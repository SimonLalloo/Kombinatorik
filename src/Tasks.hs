module Tasks (task6) where

import Data.Graph.Inductive.Graph (noNodes)
import GraphUtils (avg, avgDegree, degrees, diameter, genGaltonWatson, genRandomTree, maxDegree, minDegree, poisson)
import System.Random (randomRIO)
import Visualization (makeHistogram, visualizeDirectedUnlabelledGraph)

----------------------------------------
----------------- Task 6----------------
----------------------------------------

task6 :: Int -> IO ()
task6 n = do
    putStrLn "Running task 6..."

    values <- makeTask6Data n
    let (part1, part2) = unzip values
        (avgDeg, minDeg) = unzip part1
        (maxDeg, diam) = unzip part2

    let (gwDiam, rDiam) = unzip diam
    makeHistogram gwDiam "Galton-Watson tree diameter"
    makeHistogram rDiam "Random tree diameter"
    let a1 = avg gwDiam
        a2 = avg rDiam
    putStrLn $ "Average tree diameters are " ++ show a1 ++ " and " ++ show a2

    let (gwAvgDeg, rAvgDeg) = unzip avgDeg
        gwAvgDeg' = map (\n -> realToFrac (round (n * 10)) / 10.0) gwAvgDeg
        rAvgDeg' = map (\n -> realToFrac (round (n * 10)) / 10.0) rAvgDeg
    makeHistogram gwAvgDeg' "Galton-Watson tree average degrees"
    makeHistogram rAvgDeg' "Random tree average degrees"
    let a1 = avg gwAvgDeg'
        a2 = avg rAvgDeg'
    putStrLn $ "Average average degrees are " ++ show a1 ++ " and " ++ show a2

    let (gwMinDeg, rMinDeg) = unzip minDeg
    makeHistogram gwMinDeg "Galton-Watson tree minimum degrees"
    makeHistogram rMinDeg "Random tree minimum degrees"
    let a1 = avg gwMinDeg
        a2 = avg rMinDeg
    putStrLn $ "Average minimum degrees are " ++ show a1 ++ " and " ++ show a2

    let (gwMaxDeg, rMaxDeg) = unzip maxDeg
    makeHistogram gwMaxDeg "Galton-Watson tree maximum degrees"
    makeHistogram rMaxDeg "Random tree maximum degrees"
    let a1 = avg gwMaxDeg
        a2 = avg rMaxDeg
    putStrLn $ "Average maximum degrees are " ++ show a1 ++ " and " ++ show a2

makeTask6Data :: Int -> IO [(((Double, Double), (Int, Int)), ((Int, Int), (Int, Int)))]
makeTask6Data 0 = return []
makeTask6Data n = do
    gwTree <- genGaltonWatson $ poisson 0.75
    rTree <- genRandomTree $ noNodes gwTree

    let avgDeg = (avgDegree gwTree, avgDegree rTree)
        minDeg = (minDegree gwTree, minDegree rTree)
        maxDeg = (maxDegree gwTree, maxDegree rTree)
        diam = (diameter gwTree, diameter rTree)

    do
        xs <- makeTask6Data $ n - 1
        return $ ((avgDeg, minDeg), (maxDeg, diam)) : xs
