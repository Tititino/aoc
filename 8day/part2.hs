-- this is code is kinda shit, i wanted to do it in Erlang but
-- i lot too many hours on it and i finally decided to go back
-- to good 'ol haskell to kinda brute force it
-- might still go back and try to finish this in Erlang with
-- some concurrency

import System.Environment (getArgs)
import Data.List

ind_to_c :: Int -> Char
ind_to_c i
        | i == 0    = 'a'
        | i == 1    = 'b'
        | i == 2    = 'c'
        | i == 3    = 'd'
        | i == 4    = 'e'
        | i == 5    = 'f'
        | i == 6    = 'g'
        | otherwise = error "bad ind"
        
decode_word :: [Char] -> [Char] -> [Char]
decode_word [] _ = []
decode_word (x:xs) p = (ind_to_c ind) : (decode_word xs p)
                     where ind = case elemIndex x p of (Just n) -> n
                                                       _        -> error "???"
is_valid :: [Char] -> Bool
is_valid t = hex_to_int t >= 0

hex_to_int :: [Char] -> Int
hex_to_int h = case sort h of "abcefg"  -> 0
                              "cf"      -> 1
                              "acdeg"   -> 2
                              "acdfg"   -> 3
                              "bcdf"    -> 4
                              "abdfg"   -> 5
                              "abdefg"  -> 6
                              "acf"     -> 7
                              "abcdefg" -> 8
                              "abcdfg"  -> 9
                              _         -> -1

-- somewhere this gets flipped, here it gets unflipped
fromDigits :: [Int] -> Int
fromDigits x = foldl (\acc x -> (10 * acc) + x) 0 r
             where r = reverse x

check_perm :: [[Char]] -> [Char] -> Bool
check_perm [] _ = True
check_perm (s:strs) p = (is_valid . decode_word s $ p) && check_perm strs p

apply_perm :: [[Char]] -> [Char] -> [Int]
apply_perm [] _ = []
apply_perm (x:xs) p = (hex_to_int . decode_word x $ p) : (apply_perm xs p)

final :: [[Char]] -> Int
final strs = fromDigits . apply_perm res $ fperm
           where fperm = head . filter (check_perm strs) $ permutations "abcdefg"
                 res   = take 4 (reverse strs)

split_in :: [Char] -> [[Char]]
split_in = filter (\x -> x /= "|") . words

main = do
        args <- getArgs
        text <- readFile (args !! 0)
        print (foldl (\acc x -> acc + (final . split_in $ x)) 0 (lines text))
