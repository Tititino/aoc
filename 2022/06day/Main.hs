import Data.List
import System.Environment
import qualified Data.Text as Text
import Control.Monad
import Control.Monad.State

type Count = State ([Char], [Char]) Integer

findMarker :: Int -> Count
findMarker n = do
        (seen, toSee) <- get
        if null toSee then
                return $ length seen
        else
                let el   = head toSee in
                let rest = tail toSee in
                modify (if el `elem` seen then ([el], rest) else (el:seen, rest))

main :: IO ()
main = do
        return ()
        fileLine <- getArgs >>= readFile . head >>= return . head . lines 
        putStrLn . show . findMarker fileLine $ 4
        putStrLn . show . findMarker fileLine $ 14

