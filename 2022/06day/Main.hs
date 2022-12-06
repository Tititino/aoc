module Main where

import Data.List
import System.Environment
import qualified Data.Text as Text

findMarker :: Eq a => [a] -> Int -> Int
findMarker l n = aux 0 l
        where aux len l = if length l >= n then if ((==n) . length . nub . take n $ l) then len + n else aux (len + 1) (tail l) else -1

main :: IO ()
main = do
        fileLine <- getArgs >>= readFile . head >>= return . head . lines 
        putStrLn . show . findMarker fileLine $ 4
        putStrLn . show . findMarker fileLine $ 14

