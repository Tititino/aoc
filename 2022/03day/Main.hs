module Main where

import System.Environment (getArgs)
import Data.List (intersect)

charToNum :: Char -> Integer
charToNum c
        | c >= 'a' && c <= 'z' = (toInteger . fromEnum $ c) - (toInteger . fromEnum $ 'a') + 1
        | c >= 'A' && c <= 'Z' = (toInteger . fromEnum $ c) - (toInteger . fromEnum $ 'A') + 27
        | otherwise            = 0

findBadge :: [String] -> String
findBadge (x:y:z:rest) = (head . intersect x . intersect y $ z):(findBadge rest)
findBadge _            = []

splitInHalf :: [a] -> ([a], [a])
splitInHalf l = splitAt (length l `div` 2) l

main :: IO ()
main = do
        file <- getArgs >>= (readFile . head)
        let v1 = sum . map ((\x -> if not (null x) then charToNum . head $ x else 0) . (\(a, b) -> filter (`elem` a) b) . splitInHalf) . filter (not . null) . lines $ file
        let v2 = sum . map charToNum . findBadge . filter (not . null) . lines $ file
        putStrLn (show v1)
        putStrLn (show v2)
