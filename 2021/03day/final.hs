import System.Environment (getArgs)

data Root = RootOf (S_tree, S_tree) deriving (Show, Read)
data S_tree = Bin (S_tree, S_tree) | EmptyTree deriving (Show, Read)
data Binary = One | Zero deriving (Show, Read)

class CanBeBinned a where
        to_bin :: a -> Binary
        from_bin :: Binary -> a

instance CanBeBinned Int where
        to_bin 0 = Zero
        to_bin 1 = One
        to_bin _ = error "Not a binary digit"
        from_bin Zero = 0
        from_bin One  = 1

instance CanBeBinned Char where
        to_bin '0' = Zero
        to_bin '1' = One
        to_bin _   = error "Not a binary digit"
        from_bin Zero = '0'
        from_bin One  = '1'

empty_root :: Root
empty_root = RootOf (EmptyTree, EmptyTree)

update_s_tree :: Root -> [Binary] -> Root
update_s_tree r [] = r
update_s_tree (RootOf (zero_tree, one_tree)) (x:xs) = case x of Zero -> RootOf (update_s_tree' zero_tree xs, one_tree)
                                                                One  -> RootOf (zero_tree, update_s_tree' one_tree xs)

update_s_tree' :: S_tree -> [Binary] -> S_tree
update_s_tree' EmptyTree b = case b of []   -> Bin(EmptyTree, EmptyTree)
                                       x:xs -> case x of Zero -> Bin (update_s_tree' EmptyTree xs, EmptyTree)
                                                         One  -> Bin (EmptyTree, update_s_tree' EmptyTree xs)
update_s_tree' (Bin (zero_tree, one_tree)) b = case b of []   -> Bin (zero_tree, one_tree)
                                                         x:xs -> case x of Zero -> Bin (update_s_tree' zero_tree xs, one_tree)
                                                                           One  -> Bin (zero_tree, update_s_tree' one_tree xs)
-- ugly as shit                                                            
arr_from_tree :: Root -> [[Binary]]
arr_from_tree (RootOf (zero_tree, one_tree)) = zero ++ one
                                             where zero = case zero_tree of EmptyTree -> []
                                                                            _         -> map (Zero :) (arr_from_tree' zero_tree)
                                                   one  = case one_tree  of EmptyTree -> []
                                                                            _         -> map (One :)(arr_from_tree' one_tree)
arr_from_tree' :: S_tree -> [[Binary]]
arr_from_tree' EmptyTree = [[]]
arr_from_tree' (Bin (EmptyTree, EmptyTree)) = [[]]
arr_from_tree' (Bin (zero_tree, one_tree)) = zero ++ one
                                          where zero = case zero_tree of EmptyTree -> []
                                                                         _         -> map (Zero :) (arr_from_tree' zero_tree)
                                                one  = case one_tree  of EmptyTree -> []
                                                                         _         -> map (One :) (arr_from_tree' one_tree)

to_dec :: [Binary] -> Int
to_dec b = sum . zipWith (*) [ 2^x | x <- [len, len - 1..0] ] . map (from_bin :: Binary -> Int) $ b
        where len = length b - 1

get_common :: (Int -> Int -> Bool) -> [[Binary]] -> [Binary]
get_common [] = []
get_common f (x:[]) = x
get_common f bins = digit : (get_common f . arr_from_tree $ nexttree)  
                     where digit    = get_digit f bins
                           tree     = problem_tree bins
                           nexttree = case tree of RootOf (zero_tree, one_tree) -> case digit of Zero -> case zero_tree of EmptyTree  -> empty_root
                                                                                                                           Bin (a, b) -> RootOf(a, b)
                                                                                                 One  -> case one_tree of  EmptyTree  -> empty_root
                                                                                                                           Bin (a, b) -> RootOf(a, b)
                                                                                                                           
get_digit :: (Int -> Int -> Bool) -> [[Binary]] -> Binary
get_digit f bins = if f (foldl (\acc x -> acc + from_bin (head x)) 0 bins) halflen then One else Zero
                 where halflen = if mod (length bins) 2 == 0 then div (length bins) 2 else div (length bins) 2 + 1

problem_tree :: [[Binary]] -> Root
problem_tree bins = foldl (\acc x -> update_s_tree acc x) empty_root bins

main = do
        args <- getArgs
        text <- readFile (args !! 0)
        let x = map (\x -> map to_bin x) . lines $ text  in
                print ((to_dec . get_common (>=)  $ x) * (to_dec . get_common (<) $ x))
