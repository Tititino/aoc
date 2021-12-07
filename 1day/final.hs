import System.Environment (getArgs)

count_incs :: Ord a => [a] -> Int
count_incs xs = case xs of []     -> 0
                           x:xs   -> fst (foldl lambda (0, x) xs)
                                     where lambda x y = let sum = fst x; prev = snd x in
                                                        if prev < y
                                                        then (sum + 1, y)
                                                        else (sum, y)
                                                        
get_three :: [Int] -> [Int]
get_three xs = case xs of x:y:z:xs -> (x + y + z) : get_three (y:z:xs)
                          _        -> []

final :: [Char] -> Int
final = count_incs . map (\x  -> read x :: Int) . lines

final' :: [Char] -> Int
final' = count_incs . get_three . map (\x -> read x :: Int) . lines

main = do
        args <- getArgs
        text <- readFile (args !! 0)
        print (final' text)
