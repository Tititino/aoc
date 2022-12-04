import System.Environment ( getArgs )
import Data.Maybe

type Assignment = (Integer, Integer)

predOnLines :: [String] -> (String -> Maybe a) -> (a -> Bool) -> [Bool]
predOnLines line digestFunc predFunc = catMaybes (map (\x -> digestFunc x >>= (return . predFunc)) line)

digestAssignment :: String -> Maybe (Assignment, Assignment)
digestAssignment s = do
                        (a, (_:b))   <- return $ break (==',') s
                        (as, (_:ae)) <- return $ break (=='-') a
                        (bs, (_:be)) <- return $ break (=='-') b
                        return ((read as :: Integer, read ae :: Integer), (read bs :: Integer, read be :: Integer))

doCompletelyOverlap :: (Assignment, Assignment) -> Bool
doCompletelyOverlap ((as, ae), (bs, be)) = ((as <= bs && ae >= be) || (bs <= as && be >= ae))

doOverlap :: (Assignment, Assignment) -> Bool
doOverlap ((as, ae), (bs, be)) = ((as <= bs && ae >= bs) || (bs <= as && be >= as))

main :: IO ()
main = do
        fileLines <- getArgs >>= (readFile . head) >>= return . lines
        putStrLn . show . sum . map fromEnum $ predOnLines fileLines digestAssignment doCompletelyOverlap
        putStrLn . show . sum . map fromEnum $ predOnLines fileLines digestAssignment doOverlap
