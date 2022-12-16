module Main where

import System.Environment ( getArgs )
import Data.Maybe
import Text.Read
import Data.List
import Data.Ord
import qualified Data.Set as Set

data Sensor = Sensor { centre :: (Integer, Integer) 
                     , radius :: Integer
                     , miny   :: Integer
                     , maxy   :: Integer } deriving (Show)

instance Read Sensor where
        readsPrec _ str = case words str of
                                ["Sensor", "at", 'x':'=':sx, 'y':'=':sy, "closest", "beacon", "is", "at", 'x':'=':bx, 'y':'=':by] ->
                                        let snx = read (init sx) :: Integer in
                                        let sny = read (init sy) :: Integer in
                                        let bnx = read (init bx) :: Integer in
                                        let bny = read  by :: Integer in
                                        let r   = manhattan (snx, sny) (bnx, bny) in
                                        [(Sensor { centre = (snx, sny)
                                               , radius = r
                                               , miny   = sny - r
                                               , maxy   = sny + r }, "")]
                                _ -> []

newtype Span = Span (Integer, Integer) deriving (Show, Eq)

merge :: [Span] -> [Span]
merge [] = []
merge ((Span (a1, b1)):(Span (a2, b2)):rest) = if b1 >= a2 && a1 <= a2 then 
        let mina = min a1 a2 in
        let maxb = max b1 b2 in
        merge ((Span (mina, maxb)):rest) 
        else (Span (a1, b1)):(merge ((Span (a2, b2)):rest))
merge ((Span (a, b)):_) = [Span (a, b)]

insertSorted :: Span -> [Span] -> [Span]
insertSorted s [] = [s]
insertSorted (Span (a1, b1)) ((Span (a2, b2)):rest) = if a1 <= a2 then ((Span (a1, b1)):(Span (a2, b2)):rest) else (Span (a2, b2)):(insertSorted (Span (a1, b1)) rest)

lengthSpans :: [Span] -> Integer
lengthSpans = foldl (\acc (Span (a, b)) -> acc + (b - a)) 0

manhattan :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

sensorsOnLine :: [Sensor] -> Integer -> [Span]
sensorsOnLine s n = foldl (\acc x -> merge (insertSorted (span x) acc)) [] . filter (\se -> (maxy se) >= n && (miny se) <= n) $ s
                where span s = let (cx, cy) = centre s in
                               let dist = (radius s) - (abs (n - cy)) in
                               let minx = cx - dist in
                               let maxx = cx + dist in
                               Span (minx, maxx)

sensorsOnLine' :: [Sensor] -> Integer -> [Span]
sensorsOnLine' s n = foldl (\acc x -> merge (insertSorted x acc)) [] . map (clampSpan . span)  . filter (\se -> (maxy se) >= n && (miny se) <= n) $ s
                where span s = let (cx, cy) = centre s in
                               let dist = (radius s) - (abs (n - cy)) in
                               let minx = cx - dist in
                               let maxx = cx + dist in
                               Span (minx, maxx)
clampSpan :: Span -> Span
clampSpan (Span (a, b)) = Span (clamp (0, 4000000) a, clamp (0, 4000000) b)

findGap :: Integer -> Integer -> [Sensor] -> (Integer, [Span])
findGap from to s
        | from == to    = (to, [])
        | otherwise = let l = sensorsOnLine' s from in
                      if length l > 1 then (from, l) else findGap (from + 1) to s

main :: IO ()
main = do
        sensors <- getArgs >>= (readFile . head) >>= return . catMaybes . map (\x -> readMaybe x :: Maybe Sensor) . lines
        num <- getArgs >>= return . (\x -> read x :: Integer) . (!! 1)
        putStrLn . show $ filter (\se -> (maxy se) >= num && (miny se) <= num) sensors
        let res  = sensorsOnLine sensors num
        let res' = findGap 0 4000000 sensors
        putStrLn . show $ lengthSpans res
        putStrLn . show $ res'

