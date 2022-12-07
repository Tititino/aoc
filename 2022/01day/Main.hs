module Main where 

import Control.Monad
import System.Environment (getArgs)
import Data.List (sort, groupBy)

main :: IO ()
main = do
        cals <- longFunc . lines <$> (getArgs >>= (readFile . head))
        putStrLn . (++) "solution to 1: " . show . maximum $ cals
        putStrLn . (++) "solution to 2: " . show . sum . take 3 . reverse . sort $ cals
                where longFunc = map (sum . (map (\x -> read x :: Integer) . tail)) . groupBy (\_ b -> b /= "")
