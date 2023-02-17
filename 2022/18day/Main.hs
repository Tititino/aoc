module Main where

import Data.List.Split ( wordsBy )
import System.Environment ( getArgs )
import Data.Maybe ( catMaybes )
import Text.Read ( readMaybe )
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.State.Lazy ( modify
                                , get
                                , evalState 
                                , State )
import Data.Ix ( inRange )

newtype Coord3 = Coord3 (Integer, Integer, Integer) deriving (Eq, Ord)
type Space = S.Set Coord3

instance Read Coord3 where
        readsPrec _ str = case wordsBy (==',') str of
                                [x, y, z] -> [(Coord3 (read x :: Integer, read y :: Integer, read z :: Integer), "")]
                                _         -> []

instance Show Coord3 where
        show (Coord3 (x, y, z)) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

exposedFaces :: Space -> Coord3 -> Integer
exposedFaces s (Coord3 (x, y, z)) =
        sum . map (toInteger . fromEnum . not) $ [ S.member (Coord3 (x - 1, y, z)) s
                                                 , S.member (Coord3 (x + 1, y, z)) s
                                                 , S.member (Coord3 (x, y - 1, z)) s
                                                 , S.member (Coord3 (x, y + 1, z)) s
                                                 , S.member (Coord3 (x, y, z - 1)) s
                                                 , S.member (Coord3 (x, y, z + 1)) s ]

exposedFaces' :: Space -> Coord3 -> Integer
exposedFaces' s (Coord3 (x, y, z)) =
        sum . map (toInteger . fromEnum) $ [ S.member (Coord3 (x - 1, y, z)) s
                                           , S.member (Coord3 (x + 1, y, z)) s
                                           , S.member (Coord3 (x, y - 1, z)) s
                                           , S.member (Coord3 (x, y + 1, z)) s
                                           , S.member (Coord3 (x, y, z - 1)) s
                                           , S.member (Coord3 (x, y, z + 1)) s ]


uglySolution :: Coord3 -> Space -> State Space Integer
uglySolution c@(Coord3 (x, y, z)) s
        | any (not . inRange (-10, 30)) [x, y, z] = return 0
        | S.member c s = return 0
        | otherwise    = do
                        seen <- get
                        if (S.member c seen) then return 0
                        else do
                                modify (S.insert c)
                                t1 <- uglySolution (Coord3 (x + 1, y, z)) s
                                t2 <- uglySolution (Coord3 (x - 1, y, z)) s
                                t3 <- uglySolution (Coord3 (x, y + 1, z)) s
                                t4 <- uglySolution (Coord3 (x, y - 1, z)) s
                                t5 <- uglySolution (Coord3 (x, y, z + 1)) s
                                t6 <- uglySolution (Coord3 (x, y, z - 1)) s
                                return ((exposedFaces' s c) + t1 + t2 + t3 + t4 + t5 + t6)

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . catMaybes . map (\x -> readMaybe x :: Maybe Coord3) . lines
        let space = foldl (\acc x -> S.insert x acc) S.empty fileLines
        let res1  = foldl (\acc x -> acc + (exposedFaces space x)) 0 space
        let res2  = evalState (uglySolution (Coord3 (0, 0, 0)) space) S.empty
        putStrLn . ("part 1: " ++) . show $ res1
        putStrLn . ("part 2: " ++) . show $ res2
