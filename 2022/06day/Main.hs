module Main where

import Data.List
import System.Environment
import qualified Data.Text as Text
-- import Control.Monad.State.Lazy

-- type Count = State ([Char], [Char]) Integer

findMarker :: Eq a => [a] -> Int -> Int
findMarker l n = aux 0 l
        where aux len l = if length l >= n then if ((==n) . length . nub . take n $ l) then len + n else aux (len + 1) (tail l) else -1

-- findMarker :: Int -> Count
-- findMarker n = do
--         (seen, toSee) <- get
--         if null toSee then
--                 return $ length seen
--         else
--                 let el   = head toSee in
--                 let rest = tail toSee in
--                 modify (if el `elem` seen then ([el], rest) else (el:seen, rest))

main :: IO ()
main = do
        return ()
        fileLine <- getArgs >>= readFile . head >>= return . head . lines 
        putStrLn . show . findMarker fileLine $ 4
        putStrLn . show . findMarker fileLine $ 14

