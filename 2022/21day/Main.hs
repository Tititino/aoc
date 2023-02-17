module Main where

import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import System.Environment ( getArgs ) 
import Control.Monad.Identity
import Data.List.Split
import Data.Maybe

type Env  = M.Map String Expr

data Expr = Value Integer
          | Var String
          | Plus Expr Expr
          | Prod Expr Expr
          | Diff Expr Expr
          | Div Expr Expr deriving (Show, Eq)

eval :: Expr -> Reader Env Integer
-- eval :: Expr -> ExceptT String (ReaderT Env Identity) Integer
eval (Value v) = return v
eval (Var str) = do
        env <- ask
        r <- eval (fromJust $ M.lookup str env)
        return r
eval (Plus e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 + v2)
eval (Prod e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 * v2)
eval (Diff e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 - v2)
eval (Div e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        return (v1 `div` v2)


readOp :: (String, String, String) -> Maybe Expr
readOp (name1, "+", name2) = return $ Plus (Var name1) (Var name2)
readOp (name1, "*", name2) = return $ Prod (Var name1) (Var name2)
readOp (name1, "-", name2) = return $ Diff (Var name1) (Var name2)
readOp (name1, "/", name2) = return $ Div (Var name1) (Var name2)
readOp _                   = Nothing

readEntry :: String -> Maybe (String, Expr)
readEntry str = do
        [name, rest] <- pure $ splitOn ": " str
        case words rest of
                [op1, op, op2] -> do
                                expr <- readOp (op1, op, op2)
                                return (name, expr)
                [num]          -> return (name, Value (read num :: Integer))

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= pure . lines
        let env = foldl (\acc (name, expr) -> M.insert name expr acc) M.empty (catMaybes . map readEntry $ fileLines)
        let maybe = (eval (fromJust $ M.lookup "root" env))
        putStrLn . show $ (runReaderT (maybe) env)
        return ()
