module Main where

import qualified Data.Matrix as X
import qualified Data.Map as M
import qualified Data.Heap as H
import System.Environment
import Data.Maybe
import Control.Monad.State.Lazy 
import qualified Data.Set as Set
import Data.List

type Graph a = M.Map a [(a, Int)]

parse :: X.Matrix Int -> Graph (Int, Int)
parse mat = graphFromEdges edgeList
        where edgeList = map (\c -> (c, checkDirs mat c)) [ (x, y) | x <- [1..X.ncols mat], y <- [1..X.nrows mat] ]

readPosition :: Char -> Int
readPosition 'S' = readPosition 'a'
readPosition 'E' = readPosition 'z'
readPosition c   = (fromEnum c) - (fromEnum 'a')

graphFromEdges :: Ord a => [(a, [(a, Int)])] -> Graph a
graphFromEdges l = foldl (\acc (key, keys) -> M.insert key keys acc) M.empty l

checkDirs :: X.Matrix Int -> (Int, Int) -> [((Int, Int), Int)]
checkDirs mat (x, y) =
        catMaybes [ check (x, y - 1)
                  , check (x, y + 1)
                  , check (x - 1, y)
                  , check (x + 1, y) ]
        where val = X.getElem y x mat
              check (x, y) =
                let v = X.safeGet y x mat in
                if v /= Nothing then
                        if (fromJust v == val + 1) || (fromJust v <= val) then
                                Just ((x, y), 1)
                        else Nothing
                else Nothing

djikstra :: (Ord a, Show a) => Graph a -> a -> a -> (M.Map a Int, M.Map a a)
djikstra g from end = evalState (djikstra' g from end) (set, map', map, prev)
        where set  = Set.fromList (M.keys g)
              prev = M.empty
              map  = M.empty
              map' = foldl (\acc x -> if x == from then M.insert x 0 acc else M.insert x (maxBound :: Int) acc) M.empty (M.keys g)

-- beaten again by djikstra
djikstra' :: (Ord a, Show a) => Graph a -> a -> a -> State (Set.Set a, M.Map a Int, M.Map a Int, M.Map a a) (M.Map a Int, M.Map a a)
djikstra' graph from end = do
        (unvisited, heap, distance, prev) <- get
        if Set.null unvisited then 
                return $ (distance, prev)
        else 
                let (min, d) = head . sortBy (\(x1, y1) (x2, y2) -> compare y1 y2) . M.toList $ heap in
                if d == (maxBound :: Int) || min == end then
                        return $ (distance, prev)
                else do 
                        let newheap = M.delete min heap
                        let (newdist, newheap', newprev) = foldl (\(m, h, p) (x, i) -> 
                                let  distToX = fromJust $ M.lookup x h in
                                if   d + i < distToX
                                then (M.insert x (d + i) m, M.insert x (d + i) h, M.insert x min p)
                                else (m, h, p) 
                                ) (distance, newheap, prev) (filter (\(x, _) -> Set.member x unvisited) . fromJust $ M.lookup min graph)
                        let newset = Set.delete min unvisited
                        put (newset, newheap', newdist, newprev)
                        djikstra' graph from end

pathFrom :: M.Map (Int, Int) (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
pathFrom map start end 
        | start == end = []
        | otherwise    = case M.lookup end map of
                         Nothing -> []
                         Just x  -> end:(pathFrom map start x)

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . lines
        let mat = X.fromLists (map (map readPosition) fileLines)
        let graph = parse mat
        let start = head [ (x + 1, y) | x <- [0..(X.ncols mat - 1)], y <- [0..(X.nrows mat - 1)], (fileLines !! y) !! x == 'S' ]
        let end   = head [ (x + 1, y + 1) | x <- [0..(X.ncols mat - 1)], y <- [0..(X.nrows mat - 1)], (fileLines !! y) !! x == 'E' ]
        putStrLn . show $ (start, end)
        let (res, prev) = djikstra graph start end
        -- this is ugly and bad, but i was too lazy to do it good
        let min = foldl' (\acc (x, y) -> if X.getElem y x mat == 0 
                                        then
                                                let (p, _) = djikstra graph (x, y) end in
                                                let min = filter (\(c, v) -> c == end) $ M.toList p in
                                                let minh = if length min > 0 then snd . head $ min else maxBound :: Int in
                                                if minh < acc then minh else acc
                                        else acc) (maxBound :: Int) [ (x, y) | y <- [1..X.nrows mat], x <- [1..X.ncols mat] ]
        putStrLn . show $ min
