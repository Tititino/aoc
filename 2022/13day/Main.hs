module Main where

import System.Environment ( getArgs )
import Data.List
import Text.Read 
import Data.Maybe

data Rose a = Many [Rose a] | One a deriving (Eq)

instance Show a => Show (Rose a) where
        show (Many body) = "[" ++ (intercalate ", " (map (show) body)) ++ "]"
        show (One item)  = show item

instance Read a => Read (Rose a) where
        readsPrec _ ('[':rest) = let (rose, str) = readAux ([], rest) in [(Many rose, str)]
                                 where readAux (rose, [])       = (rose, [])
                                       readAux (rose, ']':rest) = (rose, rest)
                                       readAux (rose, str)      = case readsPrec 0 str of
                                                                  []                -> (rose, [])
                                                                  ((rose', str'):_) -> readAux (rose ++ [rose'], str')
        readsPrec _ (']':rest) = [(Many [], rest)]
        readsPrec _ str        = case readsPrec 0 str of
                                 []              -> if null str then [(Many [], "")] else readsPrec 0 (tail str)
                                 ((val, rest):_) -> [(One val, rest)]

instance Ord a => Ord (Rose a) where
        compare (One a) (One b)                   = compare a b
        compare (Many (a:arest)) (Many (b:brest)) = case compare a b of
                                                    EQ -> compare arest brest
                                                    c  -> c
        compare (Many []) (Many [])               = EQ
        compare (Many []) (Many _)                = LT
        compare (Many _) (Many [])                = GT
        compare (Many a) (One b)                  = compare (Many a) (Many [One b])
        compare (One a) (Many b)                  = compare (Many [One a]) (Many b)
        
main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . catMaybes . map (\x -> readMaybe x :: Maybe (Rose Integer)) . filter (not . null) . lines
        let dividers = [read "[[2]]" :: Rose Integer, read "[[6]]" :: Rose Integer]
        let res1     = sum     . map fst . filter ((==) LT . snd)           . zip [1..] . map (uncurry compare) . tuplify $ fileLines
        let res2     = product . map fst . filter ((`elem` dividers) . snd) . zip [1..] . sort $ (dividers ++ fileLines)
        putStrLn . ("part 1: " ++) . show $ res1
        putStrLn . ("part 2: " ++) . show $ res2
        where tuplify (x:y:rest) = (x, y):(tuplify rest)
              tuplify [x]        = [(x, x)]
              tuplify []         = []
