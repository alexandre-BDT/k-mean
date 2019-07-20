module Lib
    ( compressMeSenpai
    ) where

import System.Exit
import System.IO
import Control.Monad
import K_mean

compressMeSenpai :: [String] -> IO()
compressMeSenpai argv = do
    let color = checkInt (argv !! 0)
    let convergence = checkFloat (argv !! 1)
    let file = checkPath (argv !! 2)
    checkArg color convergence
    k_meanMeSenpai color convergence =<< file

checkInt :: String -> Int
checkInt x = do
    let maybeInt = readMaybe x :: Maybe Int
    case maybeInt of
        Just n -> n
        Nothing -> -1

checkFloat :: String -> Float
checkFloat x = do
    let maybeDouble = readMaybe x :: Maybe Float
    case maybeDouble of
        Just n -> n
        Nothing -> -1

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
        [(val, "")] -> Just val
        _           -> Nothing

checkArg :: Int -> Float -> IO()
checkArg color convergence
    | color == -1 = jpp
    | convergence == -1 = jpp
    | otherwise = return()

-- open a file
checkPath :: String -> IO [[Char]]
checkPath path = do
    content <- readFile path
    let linesOfFiles = lines content
    return linesOfFiles

exit    = exitWith ExitSuccess
jpp     = exitWith (ExitFailure 84)