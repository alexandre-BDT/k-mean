module K_mean
    ( k_meanMeSenpai
    ) where

import Prelude
import System.Exit
import System.IO
import Control.Monad
import Tools
import System.Random
import Algo

type Point = (Int, Int)
type Vector = (Float, Float, Float)

k_meanMeSenpai :: Int -> Float -> [[Char]] -> IO()
k_meanMeSenpai color limit line = do
    let list = loop line [] limit color
    i <- mList color $ randomRIO (0, length line - 1)
    let nb = uniq i
    if (length nb < color) then do
        k_meanMeSenpai color limit line 
        else algo color limit line i list

algo :: Int -> Float -> [[Char]] -> [Int] -> [Vector] -> IO()
algo color limit line i list = do
    let start = initStarterPoint list [] i
    kmean list start limit color color [[]] line

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

loop :: [[Char]] -> [Vector] -> Float -> Int -> [Vector]
loop lst@(x:xs) list limit color = do
    let vector = splt x ' '
    loop xs ((read (vector !! 1) :: Vector) : list) limit color
loop [] list limit color         = list

initStarterPoint :: [Vector] -> [Vector] -> [Int] -> [Vector]
initStarterPoint list starter (x:xs) = initStarterPoint list (list !! x : starter) xs
initStarterPoint list starter  []    = starter