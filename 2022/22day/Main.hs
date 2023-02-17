module Main where

import Data.List.Split ( splitWhen )
import System.Environment ( getArgs )
import qualified Data.Map as M
import Data.Char ( isDigit )
import Control.Monad.State.Lazy
import Control.Monad.Reader

type Point = (Int, Int)
type Board = M.Map Point Bool
data Direction = U | D | L | R deriving (Show, Eq)
data Rotation  = RotLeft | RotRight deriving (Show, Eq)
data Position = Pos Direction Point deriving (Show, Eq)
data Instruction = Move Int | Rot Rotation deriving (Show, Eq)

instance Enum Rotation where
        fromEnum RotLeft  = -1
        fromEnum RotRight = 1

        toEnum n
                | n >  0 = RotRight
                | n <  0 = RotLeft

instance Enum Direction where
        fromEnum R = 0
        fromEnum D = 1
        fromEnum L = 2
        fromEnum U = 3

        toEnum n
                | n `mod` 4 == 0 = R
                | n `mod` 4 == 1 = D
                | n `mod` 4 == 2 = L
                | n `mod` 4 == 3 = U

oppositeDirection :: Direction -> Direction
oppositeDirection U = D
oppositeDirection L = R
oppositeDirection R = L
oppositeDirection D = U

movePoint :: Direction -> Point -> Point
movePoint U (x, y) = (x, y - 1)
movePoint D (x, y) = (x, y + 1)
movePoint R (x, y) = (x + 1, y)
movePoint L (x, y) = (x - 1, y)

rotateDir :: Direction -> Rotation -> Direction
rotateDir d r = toEnum (fromEnum d) + fromEnum r

rotatePos :: Position -> Rotation -> Position
rotatePos (Pos dir p) r = Pos (rotateDir dir r) p

makeMap :: [String] -> Board 
makeMap lines = foldl (\acc (y, str) -> makeLine y str acc) M.empty (zip [1..] lines)
        where makeLine y line b = let notEmpty = dropWhile (==' ') line in
                                  let lenEmpty = (length line) - (length notEmpty) + 1 in
                                  snd $ foldl (\(x, acc) c -> (x + 1, M.insert (x, y) (c /= '.') acc)) (lenEmpty, b) notEmpty


parseDirs :: String -> [Instruction]
parseDirs []         = []
parseDirs ('R':rest) = (Rot RotRight):(parseDirs rest)
parseDirs ('L':rest) = (Rot RotLeft):(parseDirs rest)
parseDirs str        = (Move $ read num):(parseDirs rest)
        where (num, rest) = span isDigit str

findPsw :: ReaderT [Instruction] (State (Board, Position)) Position
findPsw = do
        is <- ask
        (board, pos) <- get
        if null is then return pos
        else do
                modify (update $ head is)
                local tail findPsw

loopback :: Board -> Position -> Position 
loopback b (Pos d p) = if M.member nextPoint b then loopback b (Pos d nextPoint) else Pos d p
        where nextPoint = movePoint (oppositeDirection d) p

updatePosition :: Int -> (Board, Position) -> (Board, Position)
updatePosition 0 (board, pos) = (board, pos)
updatePosition n (board, pos@(Pos d p@(x, y))) = case M.lookup nextPoint board of
                                                        Just True  -> (board, pos)
                                                        Just False -> updatePosition (n - 1) (board, Pos d nextPoint)
                                                        Nothing    -> error "idk"
                                                where shiftedPoint       = movePoint d p
                                                      (Pos _ loopedBack) = loopback board (Pos d shiftedPoint)
                                                      nextPoint          = if M.member shiftedPoint board then shiftedPoint else loopedBack


update :: Instruction -> (Board, Position) -> (Board, Position)
update (Rot r)  (board, pos) = (board, rotatePos pos r)
update (Move n) (board, pos) = updatePosition n (board, pos)

getStart :: [String] -> Point
getStart (s:_) = ((length s) - (length (dropWhile (==' ') s)), 1)

solution :: Position -> Int
solution (Pos dir (x, y)) = (y * 1000) + (4 * x) + (fromEnum dir)

main :: IO ()
main = do
        [mapStr, dirsStr] <- getArgs >>= (readFile . head) >>= pure . splitWhen (=="") . lines
        let map   = makeMap mapStr
        let start = getStart mapStr
        let dirs  = parseDirs (head dirsStr)
        let res   = evalState (runReaderT findPsw dirs) (map, Pos R start)
        -- putStrLn . show $ map
        -- putStrLn . show $ dirs
        putStrLn . show $ solution res
        return ()
