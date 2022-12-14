module Main where

import qualified Data.Map as M
import Control.Monad.State.Lazy
import System.Environment ( getArgs )
import Data.List

data Particle = Rock | Sand deriving (Eq, Show, Read, Ord)
type Cave = M.Map Point Particle
data Point = Point Integer Integer deriving (Eq, Ord)

instance Read Point where
        readsPrec _ str = let (f, s) = span (/=',') str 
                          in [(Point (read f :: Integer) (read (dropWhile (==',') s) :: Integer), "")]
instance Show Point where
        show (Point x y) = show x ++ "," ++ show y

pointDiff :: Point -> Point -> [Point]
pointDiff (Point x1 y1) (Point x2 y2) = let (maxy, miny) = (max y1 y2, min y1 y2) in
                                        let (maxx, minx) = (max x1 x2, min x1 x2) in
                                        [ Point x y | x <- [minx..maxx], y <- [miny..maxy] ]

drawLines :: [Point] -> Cave -> Cave
drawLines [] m           = m
drawLines (p1:p2:rest) m = let newmap = foldl (\acc x -> M.insert x Rock acc) m (pointDiff p1 p2)
                           in drawLines (p2:rest) newmap
drawLines (p1:_) m       = m

updateCave :: Point -> Integer -> State (Integer, Cave, Point) Integer
updateCave (Point x y) max = do
        (ticks, cave, Point x1 y1) <- get
        if y1 > max then return ticks else
                case ( M.lookup (Point x1 (y1 + 1)) cave
                     , M.lookup (Point (x1 - 1) (y1 + 1)) cave
                     , M.lookup (Point (x1 + 1) (y1 + 1)) cave) of
                                (Nothing, _, _) -> do 
                                        put (ticks, cave, Point x1 (y1 + 1))
                                        updateCave (Point x y) max
                                (_, Nothing, _) -> do
                                        put (ticks, cave, Point (x1 - 1) (y1 + 1))
                                        updateCave (Point x y) max
                                (_, _, Nothing) -> do
                                        put (ticks, cave, Point (x1 + 1) (y1 + 1))
                                        updateCave (Point x y) max
                                _               -> do
                                        put (ticks + 1, M.insert (Point x1 y1) Sand cave, Point x y)
                                        updateCave (Point x y) max

updateCave' :: Point -> Integer -> State (Integer, Cave, Point) Integer
updateCave' (Point x y) max = do
        (ticks, cave, Point x1 y1) <- get
        if y1 + 1 == max + 2 then do
                put (ticks + 1, M.insert (Point x1 y1) Sand cave, Point x y)
                updateCave' (Point x y) max
        else
                case ( M.lookup (Point x1 (y1 + 1)) cave
                     , M.lookup (Point (x1 - 1) (y1 + 1)) cave
                     , M.lookup (Point (x1 + 1) (y1 + 1)) cave) of
                                (Nothing, _, _) -> do 
                                        put (ticks, cave, Point x1 (y1 + 1))
                                        updateCave' (Point x y) max
                                (_, Nothing, _) -> do
                                        put (ticks, cave, Point (x1 - 1) (y1 + 1))
                                        updateCave' (Point x y) max
                                (_, _, Nothing) -> do
                                        put (ticks, cave, Point (x1 + 1) (y1 + 1))
                                        updateCave' (Point x y) max
                                _               -> do
                                        if (Point x1 y1) == (Point x y) then return (ticks + 1) else do
                                                put (ticks + 1, M.insert (Point x1 y1) Sand cave, Point x y)
                                                updateCave' (Point x y) max

main :: IO ()
main = do
        points <- getArgs >>= (readFile . head) >>= return . map (map (\x -> read x :: Point) . (filter (/="->")) . words) . lines
        let cave = foldl (\acc x -> drawLines x acc) M.empty points
        let maxy = maximum . map (maximum . (map (\(Point x y) -> y))) $ points
        let maxx = maximum . map (maximum . (map (\(Point x y) -> x))) $ points
        let (res, (ticks, newcave, _))  = runState (updateCave (Point 500 0) maxy) (0, cave, Point 500 0)
        let (res', (_, newcave', _))  = runState (updateCave' (Point 500 0) maxy) (0, cave, Point 500 0)
        putStrLn . show $ (maxy, maxx)
        putStrLn . show $ res
        putStrLn . show $ res'
        mapM_ (putStrLn . show) [ [ case M.lookup (Point x y) newcave' of
                                        Nothing -> '.'
                                        Just Rock -> '#'
                                        Just Sand -> 'o' | x <- [400..maxx + 10]  ] | y <- [0..(maxy + 2)] ]
