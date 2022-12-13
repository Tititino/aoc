module Main where

import qualified Data.IntMap.Strict as I
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Ord

data Monkey = Monkey { items  :: [Integer]
                     , func   :: (Integer -> Integer)
                     , throws :: Integer 
                     , modulo :: Integer
                     , toTrue :: Int
                     , toFalse :: Int
                     }

type Monkeys = I.IntMap Monkey

instance Show Monkey where
        show m = "Monkey: " ++ show (items m) ++ " " ++ show (throws m)

update :: Integer -> (Integer -> Integer) -> State Monkeys Integer
update 0 _ = do
        mks <- get
        let (Down v) =  product . take 2 . sort . map Down . map throws . I.elems $ mks
        return v
update n f = do
        mks <- get
        let newmap = foldl (\acc k -> 
                let monkey = fromJust $ I.lookup k acc in
                let newmap' = foldr (\x acc' -> 
                        let newval = f ((func monkey) x) in
                        let dest   = if newval `mod` (modulo monkey) == 0 then toTrue monkey else toFalse monkey in 
                        giveItem dest newval (modulo monkey) acc') acc (items monkey) in
                I.insert k (Monkey [] 
                                   (func monkey) 
                                   ((throws monkey) + (genericLength (items monkey))) 
                                   (modulo monkey)
                                   (toTrue monkey) 
                                   (toFalse monkey)) newmap') mks [0..(I.size mks - 1)]
        put newmap
        update (n - 1) f

giveItem :: Int -> Integer -> Integer -> Monkeys -> Monkeys
giveItem ind val m mks =
        let monkey = fromJust $ I.lookup ind mks in
        -- ugly
        I.insert ind (Monkey ((val `mod` (9699690)):(items monkey))
                             (func monkey) 
                             (throws monkey) 
                             (modulo monkey) 
                             (toTrue monkey) 
                             (toFalse monkey)) mks

main :: IO ()
main = do
        let input = I.fromList [ (0, monkey0)
                               , (1, monkey1)
                               , (2, monkey2)
                               , (3, monkey3)
                               , (4, monkey4)
                               , (5, monkey5)
                               , (6, monkey6)
                               , (7, monkey7) ]
        let res  = evalState (update 20 (`div` 3)) input
        let res' = evalState (update 10000 (id)) input
        putStrLn . (++) "part 1: " . show $ res
        putStrLn . (++) "part 2: " . show $ res'

-- uglier
monkey0 = Monkey ([89, 74])                         (\x -> x * 5)  0 17 4 7
monkey1 = Monkey ([75, 69, 87, 57, 84, 90, 66, 50]) (\x -> x + 3)  0 7  3 2
monkey2 = Monkey ([55])                             (\x -> x + 7)  0 13 0 7
monkey3 = Monkey ([69, 82, 69, 56, 68])             (\x -> x + 5)  0 2  0 2
monkey4 = Monkey ([72, 97, 50])                     (\x -> x + 2)  0 19 6 5
monkey5 = Monkey ([90, 84, 56, 92, 91, 91])         (\x -> x * 19) 0 3  6 1
monkey6 = Monkey ([63, 93, 55, 53])                 (\x -> x * x)  0 5  3 1
monkey7 = Monkey ([50, 61, 52, 58, 86, 68, 97])     (\x -> x + 4)  0 11 5 4
