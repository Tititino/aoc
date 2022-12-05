module Main where 

import System.Environment (getArgs)
import Data.List
import Control.Monad
import Data.Maybe
import Data.Char (isDigit)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Read
-- import Text.RE.TDFA.String

newtype Crate = Crate Char deriving (Eq, Ord)

instance Read Crate where
        readsPrec _ ('[':c:']':rest) = [(Crate c, rest)]
        readsPrec _ _                = []

instance Show Crate where
        show (Crate c) = "[" ++ [c] ++ "]"

newtype Arrangement = Arrangement (Integer, Integer, Integer) deriving (Show, Eq, Ord)

-- slow but cooler version
-- instance Read Arrangement where
--         readsPrec _ s =
--                 let test = s ?=~ re in
--                 if matched test  then 
--                         let matches = head (s =~ re :: [[String]]) in
--                         [(Arrangement (read (matches !! 1) :: Integer, read (matches !! 2) :: Integer, read (matches !! 3) :: Integer), drop (length . head $ matches) s)]
--                 else []
--                 where re = fromJust $ compileRegex "^move ([0-9]+) from ([0-9]+) to ([0-9]+)"
instance Read Arrangement where
        readsPrec _ s = case maybe of
                        Just arr -> arr
                        Nothing  -> []
                where getNum s = 
                        let str = dropWhile (not . isDigit) s in
                        let (num, rest) = break (not . isDigit) str in
                        case num of
                        "" -> Nothing
                        n  -> Just (read n :: Integer, rest)
                      maybe = do
                        (num1, rest1) <- getNum s 
                        (num2, rest2) <- getNum rest1
                        (num3, rest3) <- getNum rest2
                        return [(Arrangement (num1, num2, num3), rest3)]


type Stack = [Crate]

type Stacks = Map.Map Integer Stack

isSeparatorString :: String -> Bool
isSeparatorString s = " 1" `isPrefixOf` s

isArrangementString :: String -> Bool
isArrangementString s = "move" `isPrefixOf` s

getCrates :: [String] -> Stacks
getCrates s = 
        let chunks = map (map ((\x -> readMaybe x :: Maybe Crate) . Text.unpack . Text.strip) . Text.chunksOf 4 . Text.pack) prelude in
        let stacks = map catMaybes . transpose $ chunks in
        fst . foldl (\(map, ix) x -> (Map.insert ix x map, ix + 1)) (Map.empty, 1) $ stacks
        where prelude = takeWhile (not . isSeparatorString) s

updateStacks :: Arrangement -> ([Crate] -> [Crate]) -> Stacks -> Maybe Stacks
updateStacks (Arrangement (num, from, to)) f s = do
        (str, fromStack) <- Map.lookup from s >>= return . splitAt numI
        toStack          <- Map.lookup to s
        return $ Map.insert to ((f str) ++ toStack) (Map.insert from fromStack s)
        where numI  = fromIntegral num
              fromI = fromIntegral from
              toI   = fromIntegral to

main :: IO ()
main = do
        fileLines <- getArgs >>= readFile . head >>= return . lines
        let stacks = getCrates . init $ fileLines 
        let arrangements = catMaybes . map (\x -> readMaybe x :: Maybe Arrangement) $ dropWhile (not . isArrangementString) fileLines 
        -- from here on things get a little bad (unchecked fromJust, heads)
        let res  = fromJust $ foldM (\acc arr -> updateStacks arr reverse acc) stacks arrangements
        let res' = fromJust $ foldM (\acc arr -> updateStacks arr id acc) stacks arrangements
        putStrLn . show . map snd . Map.toList . Map.map head $ res
        putStrLn . show . map snd . Map.toList . Map.map head $ res'
