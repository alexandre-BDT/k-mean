module Tools
    ( splt,
      checkStr,
      mList,
      removeItem
    ) where
import System.Exit
import System.IO
import Control.Monad

splt :: String -> Char -> [String]
splt [] delim = [""]
splt (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splt cs delim

checkStr :: Int -> Int -> IO()
checkStr i n
    | i == n = return ()
    | otherwise = jpp

mList :: Int -> IO a -> IO([a])
mList 0 gen = return []
mList n gen = do
    r  <- gen
    rs <- mList(n-1) gen
    return (r:rs)

removeItem :: Float -> [Float] -> [Float]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
    | otherwise = y : removeItem x ys

exit    = exitWith ExitSuccess
jpp     = exitWith (ExitFailure 84)