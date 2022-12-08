module Main where

import System.Environment ( getArgs )
import qualified Data.Matrix as Mat
import qualified Data.Vector as Vec
import Data.List

existsVec :: (a -> Bool) -> Vec.Vector a -> Bool
existsVec p v = case Vec.find p v of
                Nothing -> False
                Just _  -> True

takeWhile' :: Vec.Vector a -> (a -> Bool) -> Int
takeWhile' v p 
        | Vec.null v                                                      = 0
        | (Vec.length v >= 2) && (p (v Vec.! 0)) && (not (p (v Vec.! 1))) = 2 
        | not (p (v Vec.! 0))                                             = 0
        | (p (v Vec.! 0))                                                 = 1 + (takeWhile' (Vec.tail v) p)

checkTreeVisibility :: Mat.Matrix Int -> (Int, Int) -> Bool 
checkTreeVisibility mat (x, y) = (existsVec (>= el) colbefore) && 
                                 (existsVec (>= el) colafter') && 
                                 (existsVec (>= el) rowbefore) && 
                                 (existsVec (>= el) rowafter')
        where cols = Mat.getCol x mat 
              rows = Mat.getRow y mat 
              el  = mat Mat.! (y, x)
              (colbefore, colafter) = Vec.splitAt (y - 1) cols
              (rowbefore, rowafter) = Vec.splitAt (x - 1) rows
              colafter'             = Vec.tail colafter
              rowafter'             = Vec.tail rowafter

checkTreeScenic :: Mat.Matrix Int -> (Int, Int) -> Int
checkTreeScenic mat (x, y) = (takeWhile' (Vec.reverse colbefore) (< el)) * 
                             (takeWhile' (colafter') (< el)) *
                             (takeWhile' (Vec.reverse rowbefore) (< el))  * 
                             (takeWhile' (rowafter') (< el))
                        where cols = Mat.getCol x mat
                              rows = Mat.getRow y mat
                              el   = mat Mat.! (y, x)
                              (colbefore, colafter) = Vec.splitAt (y - 1) cols
                              (rowbefore, rowafter) = Vec.splitAt (x - 1) rows
                              colafter'             = Vec.tail colafter
                              rowafter'             = Vec.tail rowafter

checkCoords :: Mat.Matrix Int -> (Mat.Matrix Int -> (Int, Int) -> a) -> [a]
checkCoords mat f = map (f mat) coord
        where xs = Mat.ncols mat
              ys = Mat.nrows mat
              coord = [ (x, y) | x <- [1..xs], y <- [1..ys] ]

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . lines
        let mat = Mat.fromLists $ map (\y -> map (\x -> read [x] :: Int) y) fileLines
        let (visible, nvisible) = partition (==True) $ checkCoords mat checkTreeVisibility
        let max                 = maximum $ checkCoords mat checkTreeScenic 
        putStrLn . ("part 1: " ++) . show . length $ nvisible
        putStrLn . ("part 2: " ++) . show $ max
        return () 
