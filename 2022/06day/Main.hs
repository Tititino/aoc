import Data.List
import System.Environment
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust)

findMarker :: Int -> State ([Char], [Char]) Integer 
findMarker n = do
        (seen, toSee) <- get
        if null toSee then
                return $ (toInteger . length $ seen)
        else let (el, rest)   = fromJust . uncons $ toSee in
             if (length seen < (n - 1)) || (el `elem` (take (n - 1) seen)) then do
                put (el:seen, rest)
                findMarker n
             else 
                return $ (toInteger $ length (el:seen))

main :: IO ()
main = do
        return ()
        fileLine <- getArgs >>= readFile . head >>= return . head . lines 
        forM_ [4, 14] (\x -> putStrLn . show $ evalState (findMarker x) ([], fileLine))

test1 n = evalState (findMarker n) ([], "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
test2 n = evalState (findMarker n) ([], "bvwbjplbgvbhsrlpgdmjqwftvncz")
test3 n = evalState (findMarker n) ([], "nppdvjthqldpwncqszvftbrmjlhg")
test4 n = evalState (findMarker n) ([], "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
test5 n = evalState (findMarker n) ([], "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
