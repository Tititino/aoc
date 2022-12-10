module Main where

import System.Environment ( getArgs )
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import qualified Data.Set as S
import Data.List ( isPrefixOf, stripPrefix )
import Data.Maybe ( fromJust )

data Op       = Noop | Add Integer deriving (Show, Eq, Ord)
type Screen   = S.Set Integer
type RegX     = Integer
type Cycle    = Integer
type Strength = Sum Integer
-- could've used the RWS monad, just wanted to experiment with transformers
-- sorry for anyone who has to read this
type CRT a = ReaderT [Op] (WriterT Strength (StateT (RegX, Cycle, Screen) IO)) a

instance Read Op where
        readsPrec _ str
                | "noop" `isPrefixOf` str = [(Noop, fromJust $ stripPrefix "noop" str)]
                | "addx" `isPrefixOf` str = [(Add (read (head . drop 1 . words $ str) :: Integer), concat . drop 2 . words $ str)]
                | otherwise               = []

runCRT :: [Op] -> CRT a -> IO Strength
runCRT ops f = evalStateT (execWriterT (runReaderT f ops)) (1, 1, S.empty)

updtScreen :: CRT ()
updtScreen = do
        (x, cycle, screen) <- get
        let col = (cycle - 1) `mod` 40
        when (cycle /= 1 && col == 0) (printLine screen)
        if col >= (x - 1) && col <= (x + 1) 
        then modify (\(x, cycle, screen) -> (x, cycle + 1, S.insert col screen))
        else modify (\(x, cycle, screen) -> (x, cycle + 1, screen))
        where printLine line = do
                liftIO . putStrLn $ ppLine line
                modify (\(x, cycle, screen) -> (x, cycle, S.empty))

updateRegister :: CRT Integer
updateRegister = do
        (x, cycle, screen) <- get
        ops <- ask
        if null ops then do
                liftIO . putStrLn $ ppLine screen
                return 0
        else do
                tell $ aux x cycle
                case head ops of
                        Noop  -> do
                                updtScreen
                                local (tail) updateRegister >>= return
                        Add v -> do
                                updtScreen
                                tell $ aux x (cycle + 1)
                                updtScreen
                                modify (\(x, c, s) -> (x + v, c, s))
                                local (tail) updateRegister >>= return
        where trace c     = ((c - 20) `mod` 40) == 0
              aux x cycle = Sum $ x * cycle * (toInteger . fromEnum . trace $ cycle)

ppLine :: Screen -> String
ppLine scr = map (\c -> if S.member c scr then '#' else '.') [0..39] 

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . lines
        let ops = map (\x -> read x :: Op) fileLines
        res <- runCRT ops updateRegister
        putStrLn . ("part 1: " ++) . show $ getSum res
