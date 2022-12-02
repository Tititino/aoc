import System.Environment (getArgs)

data Move = Rock | Paper | Scissor deriving Eq

instance Read Move where
        readsPrec _ (c:rest)
                        | c == 'A' || c == 'X' = [(Rock, rest)]
                        | c == 'B' || c == 'Y' = [(Paper, rest)]
                        | c == 'C' || c == 'Z' = [(Scissor, rest)]
                        | otherwise            = []

instance Show Move where
        show Rock = "Rock"
        show Paper = "Paper"
        show Scissor = "Scissor"


instance Ord Move where
        compare Rock Paper = LT
        compare Paper Scissor = LT
        compare Scissor Rock = LT
        compare x y = if x == y then EQ else GT

instance Enum Move where
        toEnum 1 = Rock
        toEnum 2 = Paper
        toEnum 3 = Scissor

        fromEnum Rock = 1
        fromEnum Paper = 2
        fromEnum Scissor = 3

        succ Rock = Paper
        succ Paper = Scissor
        succ Scissor = Rock

        pred Rock = Scissor
        pred Paper = Rock
        pred Scissor = Paper

newtype Result = Result Ordering deriving (Eq, Ord)

instance Read Result where
        readsPrec _ ('X':rest) = [(Result LT, rest)]
        readsPrec _ ('Y':rest) = [(Result EQ, rest)]
        readsPrec _ ('Z':rest) = [(Result GT, rest)]
        readsPrec _ _ = []

instance Show Result where
        show (Result LT) = "X"
        show (Result EQ) = "Y"
        show (Result GT) = "Z"

instance Enum Result where
        toEnum 0 = (Result LT)
        toEnum 3 = (Result EQ)
        toEnum 6 = (Result GT)

        fromEnum (Result LT) = 0
        fromEnum (Result EQ) = 3
        fromEnum (Result GT) = 6

moveValue :: Move -> Integer
moveValue = toInteger . fromEnum

roundValue :: Result -> Integer
roundValue = toInteger . fromEnum 

score :: (Move, Move) -> (Integer, Integer)
score (x, y) = (points x y, points y x)
        where points x y = moveValue x + (roundValue . Result . compare x $ y)

score' :: (Move, Result) -> (Integer, Integer)
score' (m, Result EQ) = let s = moveValue m + roundValue (Result EQ) in (s, s)
score' (m, Result GT) = let winningMove = succ m in score (m, winningMove)
score' (m, Result LT) = let losingMove = pred m in score (m, losingMove)

main :: IO ()
main = do
        file <- getArgs >>= (readFile . head)
        let wordsFiltered readFunc = map (readFunc . (filter (\x -> length x /= 2) . words)) . lines
        let countFinalScore readFunc scoreFunc = sum . map (snd . scoreFunc) . (wordsFiltered readFunc) 
        putStrLn . (++) "first part: "  . show . countFinalScore (\(x:y:_) -> (read x :: Move, read y :: Move))   score  $ file
        putStrLn . (++) "second part: " . show . countFinalScore (\(x:y:_) -> (read x :: Move, read y :: Result)) score' $ file
