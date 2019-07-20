module Algo
    ( kmean
    ) where

import Prelude
import System.Exit
import System.IO
import Control.Monad
import Tools
import System.Random
import Debug.Trace
import Data.List

type Point = (Int, Int)
type Vector = (Float, Float, Float)

kmean :: [Vector] -> [Vector] -> Float -> Int -> Int -> [[[Char]]] -> [[Char]] -> IO()
kmean list centroids limit color nColor points line = do
    if color == 0 then stepTwo points centroids list limit nColor line []--printMean points line centroids
    else kmean list centroids limit (color - 1) nColor ((findNearest line (centroids !! (color - 1)) centroids []) : points) line

stepTwo :: [[[Char]]] -> [Vector] -> [Vector] -> Float -> Int -> [[Char]] -> [Vector] -> IO()
stepTwo points@(x:xs) centroids list limit color line new = do
    let lst = loop x []
    let nb = center lst
    if checkCentroids centroids nb limit == 1 then finalPrint points centroids 0 color else stepTwo xs centroids list limit color line (nb : new)
stepTwo [] centroids list limit color line new             = do
    let lst = drop 1 new
    kmean list lst limit color color [[]] line

finalPrint :: [[[Char]]] -> [Vector] -> Int -> Int -> IO()
finalPrint points lst@(x:xs) index color = do
    if color - 1 == index
        then do
            putStrLn "--" >> print x >> putStrLn "-"
            printPoints (points !! index)
            exit
        else do
            putStrLn "--" >> print x >> putStrLn "-"
            printPoints (points !! index)
            finalPrint points xs (index + 1) color

printPoints :: [[Char]] -> IO()
printPoints lst@(x:xs) = putStrLn x >> printPoints xs
printPoints []         = return ()

checkCentroids :: [Vector] -> Vector -> Float -> Int
checkCentroids lst@(x:xs) new limit
    | computeToEuclidian x new < limit = 1
    | otherwise                        = 0
checkCentroids [] _ _                  = 0

loop :: [[Char]] -> [Vector] -> [Vector]
loop lst@(x:xs) list = do
    let vector = splt x ' '
    loop xs ((read (vector !! 1) :: Vector) : list)
loop [] list         = list

findNearest :: [[Char]] -> Vector -> [Vector] -> [[Char]] -> [[Char]]
findNearest lst@(y:ys) centroid centroids nearest = 
    if (comparePoints (computeToEuclidian centroid (read ((splt y ' ') !! 1) :: Vector)) (read ((splt y ' ') !! 1) :: Vector) centroids [] == 1) then do
    findNearest ys centroid centroids (y : nearest) else findNearest ys centroid centroids nearest
findNearest [] _ _ nearest                        = nearest

comparePoints :: Float -> Vector -> [Vector] -> [Float] -> Int
comparePoints main point lst@(x:xs) list = comparePoints main point xs ((computeToEuclidian point x) : list)
comparePoints _ _ [] []    = 0
comparePoints main point [] list
    | main <= minimum list = 1
    | otherwise            = 0

center :: [Vector] -> Vector
center ps = let (xs, ys, yz) = unzip3 ps in (average xs, average ys, average yz)

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

computeToEuclidian :: Vector -> Vector -> Float
computeToEuclidian (a1, a2, a3) (b1, b2, b3) = sqrt (x'*x' + y'*y' + z'*z')
    where
        x' = b1 - a1
        y' = b2 - a2
        z' = b3 - a3

exit    = exitWith ExitSuccess
