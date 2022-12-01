
import System.Environment (getArgs)

data Expr = 
          Value Int
        | Inp Int (Int -> Int)
        | Add Expr Expr
        | Mul Expr Expr
        | Div Expr Expr
        | Mod Expr Expr
        | Eql Expr Expr
        
instance Show Expr where
        show (Value n) = show n
        show (Add expr1 expr2) = "Add (" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
        show (Mul expr1 expr2) = "Mul (" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
        show (Div expr1 expr2) = "Div (" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
        show (Mod expr1 expr2) = "Mod (" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
        show (Eql expr1 expr2) = "Eql (" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
        show (Inp i f)         = "Inp (" ++ show i ++ ")"

type State = (Expr, Expr, Expr, Expr, Int)

parseOperator :: [String] -> State -> State
parseOperator ["inp", reg]     state@(w, x, y, z, n) = updateState (w, x, y, z, n + 1) reg (\_ -> Inp n id)
parseOperator ["add", reg, op] state@(w, x, y, z, n) = updateState state reg (\x -> Add x (getOperator state op))
parseOperator ["mul", reg, op] state@(w, x, y, z, n) = updateState state reg (\x -> Mul x (getOperator state op))
parseOperator ["div", reg, op] state@(w, x, y, z, n) = updateState state reg (\x -> Div x (getOperator state op))
parseOperator ["mod", reg, op] state@(w, x, y, z, n) = updateState state reg (\x -> Mod x (getOperator state op))
parseOperator ["eql", reg, op] state@(w, x, y, z, n) = updateState state reg (\x -> Eql x (getOperator state op))

updateState :: State -> String -> (Expr -> Expr) -> State
updateState (w, x, y, z, n) "w" f = (f w, x, y, z, n)
updateState (w, x, y, z, n) "x" f = (w, f x, y, z, n)
updateState (w, x, y, z, n) "y" f = (w, x, f y, z, n)
updateState (w, x, y, z, n) "z" f = (w, x, y, f z, n)

parseOperatorOpt :: [String] -> State -> State
parseOperatorOpt st = stateOpt . parseOperator st

stateOpt :: State -> State
stateOpt (w, x, y, z, n) = (optimizeOperation2 w, optimizeOperation2 x, optimizeOperation2 y, optimizeOperation2 z, n)


getOperator :: State -> String -> Expr
getOperator (w, _, _, _, _) "w" = w
getOperator (_, x, _, _, _) "x" = x
getOperator (_, _, y, _, _) "y" = y
getOperator (_, _, _, z, _) "z" = z
getOperator (_, _, _, _, _) str = Value (read str :: Int)

optimizeOperation2 :: Expr -> Expr
optimizeOperation2 (Mul e (Value 0)) = Value 0
optimizeOperation2 (Mul e (Value 1)) = e
optimizeOperation2 (Add e (Value 0)) = e
optimizeOperation2 (Div e (Value 1)) = e
optimizeOperation2 (Mul (Value 1) e) = e
optimizeOperation2 (Mul (Value 0) e) = Value 0
optimizeOperation2 (Add (Value 0) e) = e
optimizeOperation2 (Div (Value 0) _) = Value 0
optimizeOperation2 (Div (Inp i f) (Value n)) = if n <= -10 || n >= 10 then Value 0 else Inp i ((`div` n) . f) 
optimizeOperation2 (Mod (Inp i f) (Value n)) = if n >= 10 then Inp i f else Inp i ((`mod` n) . f)
optimizeOperation2 (Eql (Value v) (Inp i f)) = if v < 1 || v > 9 then Value 0 else Eql (Value v) (Inp i f)
optimizeOperation2 (Eql (Inp i f) (Value v)) = if v < 1 || v > 9 then Value 0 else Eql (Inp i f) (Value v)
optimizeOperation2 (Add (Inp i f) (Value v)) = Inp i (f . (+v))
optimizeOperation2 (Mul (Inp i f) (Value v)) = Inp i (f . (*v))
optimizeOperation2 (Mul (Value i1) (Value i2)) = Value (i1 * i2)
optimizeOperation2 (Add (Value i1) (Value i2)) = Value (i1 + i2)
optimizeOperation2 (Div (Value i1) (Value i2)) = Value (i1 `div` i2)
optimizeOperation2 (Mod (Value i1) (Value i2)) = Value (i1 `mod` i2)
optimizeOperation2 (Eql (Value i1) (Value i2)) = Value (if i1 == i2 then 1 else 0)
optimizeOperation2 expr              = expr

parseOpt :: State -> [String] -> State
parseOpt = foldl (\acc x -> parseOperatorOpt (words x) acc)

initialState :: State
initialState = (Value 0, Value 0, Value 0, Value 0, 0)

eval :: Expr -> [Int] -> Maybe Int
eval (Mul exp1 exp2) vals = 
        case (eval exp1 vals, eval exp2 vals) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> Just (a * b)
eval (Add exp1 exp2) vals = 
        case (eval exp1 vals, eval exp2 vals) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> Just (a + b)
eval (Div exp1 exp2) vals = 
        case (eval exp1 vals, eval exp2 vals) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> if b == 0 then Nothing else Just (a `div` b)
eval (Mod exp1 exp2) vals = 
        case (eval exp1 vals, eval exp2 vals) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> if a < 0 || b <= 0 then Nothing else Just (a `mod` b)
eval (Eql exp1 exp2) vals = 
        case (eval exp1 vals, eval exp2 vals) of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just a, Just b) -> Just (if a == b then 1 else 0)
eval (Value n)       vals = Just n
eval (Inp n f)       vals = Just (f $ vals !! n)

bruteForce :: Expr -> [[Int]] -> [Int]
bruteForce _ [] = []
bruteForce i (num:res) = 
        case eval i num of
        Just 0 -> num
        _      -> bruteForce i res

num :: [[Int]]
num = [[a, b, c, d, e, f, g, h, i, j, k, l, m, n] |
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

strangeEval :: State -> [String] -> [Expr]
strangeEval _ [] = []
strangeEval s@(_, _, _, _, n) (x:xs) =
        case x of
        'i':'n':'p':_ -> exp:strangeEval (Inp n id, Value 0, Value 0, Value 0, n + 1) xs
        _       -> strangeEval (parseOperatorOpt (words x) s) xs
        where (_, _, _, exp, _)  = parseOperatorOpt (words x) s

main = do
        args <- getArgs
        (_, _, _, z, n) <- fmap (parseOpt initialState . lines) . readFile . head $ args
        -- expr <- fmap (strangeEval initialState . lines) . readFile . head $ args
        _ <- print z
        print . bruteForce z $ num
        





