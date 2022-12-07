module Main where

import System.Environment ( getArgs )
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State.Lazy as State
import Control.Monad 

type Size = Integer
data Entry = File String Size | Dir String (Map.Map String Entry) deriving Show
data DirSpec = DirSpec [String] [[String]] deriving (Show, Eq)

getName :: Entry -> String
getName (File name _) = name
getName (Dir name _)  = name

parseEntry :: [String] -> (String, Entry)
parseEntry ["dir", name] = (name, Dir name Map.empty)
parseEntry [num, name]   = (name, File name (read num :: Integer))

toAbsolute :: [[String]] -> [String] -> [DirSpec]
toAbsolute [] _ = []
toAbsolute (["$", "cd", "/"]:rest) _ = (toAbsolute rest [""])
toAbsolute (["$", "cd", ".."]:rest) (_:dirs) = (toAbsolute rest dirs)
toAbsolute (["$", "cd", name]:rest) (currentDir:dirs) = 
        let absolutePath = currentDir ++ " " ++ name in
        (toAbsolute rest (absolutePath:currentDir:dirs))
toAbsolute (["$", "ls"]:rest) dirs = (DirSpec (words . head $ dirs) entries):(toAbsolute rest' dirs)
        where (entries, rest') = span ((/=)"$" . head) rest
toAbsolute rest dirs = error ((show rest) ++ " " ++ (show dirs))

dirSpecToTree :: Entry -> DirSpec -> Entry
dirSpecToTree (Dir name children) (DirSpec [] values) =
        Dir name (foldl (\acc (name, entry) -> Map.insert name entry acc) children (map parseEntry values))
dirSpecToTree (Dir name children) (DirSpec path values) =
        let newchildren = Map.insert (head path) (dirSpecToTree (fromJust $ Map.lookup (head path) children) (DirSpec (tail path) values)) children in
        Dir name newchildren
dirSpecToTree _ _ = error "bad"
        
sizeMap :: Entry -> String -> Map.Map String (Size, Bool) -> Map.Map String (Size, Bool)
sizeMap (File name size) path tree =
        Map.insert (path ++ "/" ++ name) (size, False) tree
sizeMap (Dir name children) path tree =
        let newtree = foldl (\acc x -> sizeMap x (path ++ "/" ++ name) acc) tree children in
        let childNames = map snd . Map.toList $ Map.map (\x -> (path ++ "/" ++ name ++ "/" ++ (getName x))) children in
        let size = sum $ map (\x -> fst . Map.findWithDefault (0, False) x $ newtree) childNames in
        Map.insert (path ++ "/" ++ name) (size, True) newtree

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . map words . lines
        let dirspec = toAbsolute fileLines []
        let tree    = foldl (\acc x -> dirSpecToTree acc x) (Dir "" Map.empty) dirspec
        let sizemap = Map.toList $ sizeMap tree "" Map.empty
        let space   = 30000000 - (70000000 - (fst . snd . head $ sizemap))
        putStrLn . show . sum     . map fst . filter (\(s, b) -> b && s <= 100000) . map snd $ sizemap
        putStrLn . show . minimum . map fst . filter (\(s, b) -> b && s >= space)  . map snd $ sizemap
