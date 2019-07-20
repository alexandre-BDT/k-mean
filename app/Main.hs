module Main where

import System.Environment
import System.Exit
import Lib

main :: IO()
main = getArgs >>= parse

parse :: [String] -> IO()
parse x = do
        let i = length x
        checkArg i x

checkArg :: Int -> [String] -> IO()
checkArg i argv
        | i == 3 = compressMeSenpai argv
        | otherwise = usage >> jpp

usage   = putStrLn "USAGE: ./imageCompressor n e IN\n\nn\tnumber of colors in the final image\ne\tconvergence limit\nIN\tpath to the file containing the colors of the pixels"
exit    = exitWith ExitSuccess
jpp     = exitWith (ExitFailure 84)