import System.Environment (getArgs)

data Register = W | X | Y | Z deriving Show
data Operand  = Reg Register | Val Int deriving Show
type State    = (Int, Int, Int, Int, [Int])

data Operation = Inp Register 
               | Add Register Operand
               | Mul Register Operand
               | Div Register Operand
               | Mod Register Operand
               | Eql Register Operand
               deriving Show

type Instruction = (Operation, Int) 

type Program = (Either String State, [Instruction])

apply :: Either String State -> Instruction -> Either String State
apply (Left str) _                                = Left str
apply (Right (w, x, y, z, [])) (Inp r, index)     = Left ("Input error (no more inputs) at line " ++ show index)
apply (Right state@(_, _, _, _, v:_)) (Inp r, _)  = Right (updateState state r v)
apply (Right state) (Add r o, _)                  = Right (updateState state r (getValueOfOperand state o + getValueOfRegister state r))
apply (Right state) (Mul r o, _)                  = Right (updateState state r (getValueOfOperand state o * getValueOfRegister state r))
apply (Right state) (Div r o, index)              = if den /= 0 then Right (updateState state r (getValueOfRegister state r `div` den)) else Left ("Arith error (division by zero) at line " ++ show index)
                                                    where den = getValueOfOperand state o
apply (Right state) (Mod r o, index)              = if a >= 0 && b > 0 then Right (updateState state r (a `mod` b)) else Left ("Arith error (bad modulo) at line " ++ show index)
                                                    where a = getValueOfRegister state r
                                                          b = getValueOfOperand state o
apply (Right state) (Eql r o, _)                  = Right (updateState state r (if getValueOfOperand state o == getValueOfRegister state r then 1 else 0))

updateState :: State -> Register -> Int -> State
updateState (_, x, y, z, ins) W v = (v, x, y, z, ins)
updateState (w, _, y, z, ins) X v = (w, v, y, z, ins)
updateState (w, x, _, z, ins) Y v = (w, x, v, z, ins)
updateState (w, x, y, _, ins) Z v = (w, x, y, v, ins)

getValueOfOperand :: State -> Operand -> Int
getValueOfOperand _ (Val v)               = v
getValueOfOperand (w, _, _, _, _) (Reg W) = w
getValueOfOperand (_, x, _, _, _) (Reg X) = x
getValueOfOperand (_, _, y, _, _) (Reg Y) = y
getValueOfOperand (_, _, _, z, _) (Reg Z) = z

getValueOfRegister :: State -> Register -> Int
getValueOfRegister (w, _, _, _, _) W = w
getValueOfRegister (_, x, _, _, _) X = x
getValueOfRegister (_, _, y, _, _) Y = y
getValueOfRegister (_, _, _, z, _) Z = z

getRegister :: String -> Register
getRegister "w" = W
getRegister "x" = X
getRegister "y" = Y
getRegister "z" = Z
getRegister _   = error "bad register" 

getOperand :: String -> Operand
getOperand "w" = Reg W
getOperand "x" = Reg X
getOperand "y" = Reg Y
getOperand "z" = Reg Z
getOperand str = Val (read str :: Int)

eval :: Program -> Either String State
eval (Left str, _) = Left str
eval (Right state, []) = Right state
eval (state, i:is) = eval (apply state i, is)

parseOperator :: [String] -> Operation 
parseOperator ["inp", reg]     = Inp (getRegister reg)
parseOperator ["add", reg, op] = Add (getRegister reg) (getOperand op)
parseOperator ["mul", reg, op] = Mul (getRegister reg) (getOperand op)
parseOperator ["div", reg, op] = Div (getRegister reg) (getOperand op)
parseOperator ["mod", reg, op] = Mod (getRegister reg) (getOperand op)
parseOperator ["eql", reg, op] = Eql (getRegister reg) (getOperand op)


parse :: [String] -> [Instruction] 
parse = zipWith (\i x -> (parseOperator . words $ x, i)) [0..]

bruteForce :: [Instruction] -> [[Int]] -> Either String [Int]
bruteForce _ [] = Left "None found"
bruteForce i (num:res) = 
        case eval (Right (0, 0, 0, 0, num), i) of
        Left str -> Left str
        Right (_, _, _, z, _) -> if z == 0 then Right num else bruteForce i res

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

main = do
        args <- getArgs
        program <- fmap (parse . lines) . readFile . head $ args
        print . bruteForce program $ num
