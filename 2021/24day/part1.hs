import System.Environment (getArgs)

data Var = X | Y | Z | W deriving (Show, Read)

data Operand = Variable Var | Value Int deriving (Show, Read)

data Op = Inp Var | Operation (Int -> Int -> Int)  Var Operand | Predicate (Int -> Int -> Bool) Var Operand

num :: [[Int]]
num = [ a : b : c : d : e : f : g : h : i : j : k : l : m : n : [] |
        a <- [9,8..1],
        b <- [9,8..1],
        c <- [9,8..1],
        d <- [9,8..1],
        e <- [9,8..1],
        f <- [9,8..1],
        g <- [9,8..1],
        h <- [9,8..1],
        i <- [9,8..1],
        j <- [9,8..1],
        k <- [9,8..1],
        l <- [9,8..1],
        m <- [9,8..1],
        n <- [9,8..1]
      ]

get_z :: (Int, Int, Int, Int) -> Int
get_z (_, _, z, _) = z

letter_to_var :: [Char] -> Var
letter_to_var s
        | s == "x" = X
        | s == "y" = Y
        | s == "z" = Z
        | s == "w" = W

string_to_operand :: [Char] -> Operand
string_to_operand s
                | s == "x" || s == "y" || s == "z" || s == "w" = Variable (letter_to_var s)
                | otherwise                                    = Value (read s)

get_value_from_tuple :: Var -> (Int, Int, Int, Int) -> Int
get_value_from_tuple X (x, _, _, _) = x
get_value_from_tuple Y (_, y, _, _) = y
get_value_from_tuple Z (_, _, z, _) = z
get_value_from_tuple W (_, _, _, w) = w

op_exec :: Op -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
op_exec (Operation f to (Variable from)) t@(x, y, z, w) = case to of X -> (f x val, y, z, w)
                                                                     Y -> (x, f y val, z, w)
                                                                     Z -> (x, y, f z val, w)
                                                                     W -> (x, y, z, f w val)
                                                          where val = get_value_from_tuple from t
                                                                
op_exec (Operation f to (Value n)) (x, y, z, w) = case to of X -> (f x n, y, z, w)
                                                             Y -> (x, f y n, z, w)
                                                             Z -> (x, y, f z n, w)
                                                             W -> (x, y, z, f w n)
                                                             
op_exec (Predicate p to (Variable from)) t@(x, y, z, w) = case to of X -> (if p x val then 1 else 0, y, z, w)
                                                                     Y -> (x, if p y val then 1 else 0, z, w)
                                                                     Z -> (x, y, if p z val then 1 else 0, w)
                                                                     W -> (x, y, z, if p w val then 1 else 0)
                                                          where val = get_value_from_tuple from t

op_exec (Predicate p to (Value n)) (x, y, z, w) = case to of X -> (if p x n then 1 else 0, y, z, w)
                                                             Y -> (x, if p y n then 1 else 0, z, w)
                                                             Z -> (x, y, if p z n then 1 else 0, w)
                                                             W -> (x, y, z, if p w n then 1 else 0)

exec_instructions :: [Op] -> [Int] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
exec_instructions [] _ t = t
exec_instructions ((Inp to):instr) (n:ns) t@(x, y, z, w) = exec_instructions instr ns newt
                                                           where newt = case to of X -> (n, y, z, w)
                                                                                   Y -> (x, n, z, w)
                                                                                   Z -> (x, y, n, w)
                                                                                   W -> (x, y, z, n)

exec_instructions (i:instr) n t = exec_instructions instr n newt
                                  where newt = op_exec i t

try_nums :: [[Int]] -> [Op] -> [Int]
try_nums (n:ns) ops = if res then n else try_nums ns ops
                      where res = (get_z . exec_instructions ops n $ (0, 0, 0, 0)) == 0
  
                                                              
get_ops :: [[Char]] -> [Op]
get_ops [] = []
get_ops (s:str) = operation : (get_ops str)
                  where operation = case words s of ["inp", v]        -> Inp (letter_to_var v)
                                                    ["add", to, from] -> Operation (+) (letter_to_var to) (string_to_operand from)
                                                    ["mul", to, from] -> Operation (*) (letter_to_var to) (string_to_operand from)
                                                    ["div", to, from] -> Operation (div) (letter_to_var to) (string_to_operand from)
                                                    ["mod", to, from] -> Operation (mod) (letter_to_var to) (string_to_operand from)
                                                    ["eql", to, from] -> Predicate (==) (letter_to_var to) (string_to_operand from)
                                                    _                 -> error s

test :: [[Char]] -> [Char]
test [] = ""
test (s:strs) =  case words s of ["inp", v]        -> test strs
                                 ["add", to, from] -> test strs
                                 ["mul", to, from] -> test strs
                                 ["div", to, from] -> test strs
                                 ["mod", to, from] -> test strs
                                 ["eql", to, from] -> test strs
                                 _                 -> s
  

final :: [[Char]] -> [Int]
final text = try_nums num ops
             where ops = get_ops text

main = do
        args <- getArgs
        text <- readFile (args !! 0)
        print . final . lines $ text
        -- print . test . lines $ (args !! 0)

data ChunkResult = Res Int (Int, Int, Int, Int) Chunk

memoize :: [ChunkResult] -> [Chunk] -> [Int] -> [Int]
memoize [] (c:chs) [] = memoize chunkres chs [9]
                        where chunkres = Res 9 (0, 0, 0, 0) c
memoize cs (c:chs) (n:ns) = memoize cs
  		            where newchunk = Res n (
memoize ((Res 0 _ _):cs) [] = 
memoize ((Res n t@(x, y, z, w) c):cs) [] = if (z == 0) then n else memoize ((Res (n - 1) t c):cs)
