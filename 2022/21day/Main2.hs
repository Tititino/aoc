import qualified Data.Map as M
import System.Environment ( getArgs ) 
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, fromJust )

type Env  = M.Map String Expr

data Expr = Value Integer
          | Var String
          | Plus Expr Expr
          | Prod Expr Expr
          | Diff Expr Expr
          | Div Expr Expr deriving (Show, Eq)

inverse :: Integer -> Expr -> Integer
inverse n (Plus (Value v) expr) = inverse (n - v) expr
inverse n (Plus expr (Value v)) = inverse (n - v) expr
inverse n (Prod (Value v) expr) = inverse (n `div` v) expr
inverse n (Prod expr (Value v)) = inverse (n `div` v) expr
inverse n (Diff (Value v) expr) = inverse (- (n - v)) expr
inverse n (Diff expr (Value v)) = inverse (n + v) expr
inverse n (Div (Value v) expr) = inverse (v `div` n) expr
inverse n (Div expr (Value v)) = inverse (n * v) expr
inverse n (Var _) = n
inverse _ _ = error "idk"

compress :: Env -> Expr -> Expr
comrpess _ (Plus (Value v1) (Value v2)) = Value (v1 + v2)
compress _ (Prod (Value v1) (Value v2)) = Value (v1 * v2)
compress _ (Div  (Value v1) (Value v2)) = Value (v1 `div` v2)
compress _ (Diff (Value v1) (Value v2)) = Value (v1 - v2)
compress _ (Value v) = Value v
compress _ (Var "humn") = Var "humn"
compress env (Var s) = compress env (fromJust $ M.lookup s env)
compress env (Plus e1 e2) = case (compress env e1, compress env e2) of
                                (Value v1, Value v2) -> Value (v1 + v2)
                                (e1', e2')           -> Plus e1' e2'
compress env (Prod e1 e2) = case (compress env e1, compress env e2) of
                                (Value v1, Value v2) -> Value (v1 * v2)
                                (e1', e2')           -> Prod e1' e2'
compress env (Diff e1 e2) = case (compress env e1, compress env e2) of
                                (Value v1, Value v2) -> Value (v1 - v2)
                                (e1', e2')           -> Diff e1' e2'
compress env (Div  e1 e2) = case (compress env e1, compress env e2) of
                                (Value v1, Value v2) -> Value (v1 `div` v2)
                                (e1', e2')           -> Div e1' e2'

readOp :: (String, String, String) -> Maybe Expr
readOp (name1, "+", name2) = return $ Plus (Var name1) (Var name2)
readOp (name1, "*", name2) = return $ Prod (Var name1) (Var name2)
readOp (name1, "-", name2) = return $ Diff (Var name1) (Var name2)
readOp (name1, "/", name2) = return $ Div (Var name1) (Var name2)
readOp _                   = Nothing

readEntry :: String -> Maybe (String, Expr)
readEntry str = do
        [name, rest] <- pure $ splitOn ": " str
        case name of
                "humn" -> Nothing
                _      -> case words rest of
                                [op1, op, op2] -> do
                                        expr <- readOp (op1, op, op2)
                                        return (name, expr)
                                [num]          -> return (name, Value (read num :: Integer))

trees :: Expr -> Maybe (Expr, Expr)
trees (Plus e1 e2) = Just (e1, e2)
trees (Diff e1 e2) = Just (e1, e2)
trees (Prod e1 e2) = Just (e1, e2)
trees (Div  e1 e2) = Just (e1, e2)
trees _            = Nothing

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= pure . lines
        let env = foldl (\acc (name, expr) -> M.insert name expr acc) M.empty (catMaybes . map readEntry $ fileLines)
        let (t1, t2) = fromJust $ do
                root <- (M.lookup "root" env)
                (t1, t2) <- trees root
                return (compress env $ t1, compress env $ t2)
        let res = case (t1, t2) of
                        (Value v, _) -> inverse v t2
                        (_, Value v) -> inverse v t1
                        _            -> error "idk"
        putStrLn . show $ res
        return ()
