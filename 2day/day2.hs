import System.Environment (getArgs)

fst3 :: (a, b, c) -> a
fst3 t = case t of (a, _, _) -> a

snd3 :: (a, b, c) -> b
snd3 t = case t of (_, b, _) -> b

thr3 :: (a, b, c) -> c
thr3 t = case t of (_, _, c) -> c

final :: [([Char], Int)] -> (Int, Int, Int)
final xs = case xs of [] -> (0, 0, 0)
                      x:xs
                        | word == "forward" -> (dpth + (aim * num), num + hrzt, aim)  
                        | word == "down"    -> (dpth, hrzt, aim + num)
                        | word == "up"      -> (dpth, hrzt, aim - num)
                        | otherwise         -> error "dumbass"
                        where word = fst x
                              num  = snd x
                              rest = final xs
                              dpth = fst3 rest
                              hrzt = snd3 rest
                              aim  = thr3 rest
               
res :: [Char] -> Int
res t = first * second
        where result = final . reverse . map (\x -> case x of a:b:_ -> (a, read b :: Int)) . map words . lines $ t
              first  = fst3 result
              second = snd3 result

main = do
        arg <- getArgs
        text <- readFile (arg !! 0)
        print . res $ text
