module Main where

import System.Environment ( getArgs )
import qualified Control.Monad.State.Lazy as State
import Control.Monad
import qualified Data.Set as Set

data Direction = U | D | R | L
type Rope = [(Int, Int)]
type Visits = Set.Set (Int, Int)

updateHd :: Direction -> (Int, Int) -> (Int, Int)
updateHd U (x, y) = (x, y + 1)
updateHd D (x, y) = (x, y - 1)
updateHd L (x, y) = (x - 1, y)
updateHd R (x, y) = (x + 1, y)

updateTl :: (Int, Int) -> (Int, Int) -> (Int, Int)
updateTl (hx, hy) (tx, ty) = if (abs x) > 1 || (abs y) > 1
                             then (tx + (signum x), ty + (signum y))
                             else (tx, ty)
                             where x = hx - tx
                                   y = hy - ty

mapMove :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
mapMove _ [] = []
mapMove hd (tl:rest) = 
        let newtl = updateTl hd tl in
        newtl:(mapMove newtl rest)

move :: [Direction] -> State.State (Rope, Visits) Integer
move [] = do
        (_, vs) <- State.get
        return $ toInteger . Set.size $ vs
move (dir:rest) = do
        (rope, vs) <- State.get
        let hd = updateHd dir (head rope)
        let tl = tail rope
        let newrope = mapMove hd tl
        State.put (hd:newrope, Set.insert (last newrope) vs)
        move rest

digestLines :: [[String]] -> [Direction]
digestLines [] = []
digestLines (["U", num]:rest) = (replicate (read num :: Int) U) ++ (digestLines rest)
digestLines (["D", num]:rest) = (replicate (read num :: Int) D) ++ (digestLines rest)
digestLines (["L", num]:rest) = (replicate (read num :: Int) L) ++ (digestLines rest)
digestLines (["R", num]:rest) = (replicate (read num :: Int) R) ++ (digestLines rest)

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . map words . lines
        let directions = digestLines fileLines
        forM_ [2, 10] (\x -> putStrLn . show $ State.evalState (move directions) (replicate x (0, 0), Set.empty))

