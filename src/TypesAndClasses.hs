module TypesAndClasses where
halve []   = ([], [])
halve list = if even (length list)
    then splitAt (length list `div` 2) list
    else error "Halving undefined for odd length lists"

third []     = error ""
third [x]    = error ""
third [x, y] = error ""
third list   = list !! 2

safetailCondition list = if null list then [] else tail list

safetailGuarded list | null list = []
                     | otherwise = tail list

safetailPatterns []   = []
safetailPatterns list = tail list

data Nat = Zero | Succ Nat 

nat2int :: Nat -> Int 
nat2int Zero = 0 
nat2int Succ x = 1 + nat2int x 

int2nat :: Int -> Nat
int2nat x | x < 0 = undefined
          | x == 0 = Zero 
          | otherwise = 1 + $ int2nat $ x - 1

add :: Nat -> Nat -> Nat 
add Zero n = n 
add (Succ m) n = Succ (add m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: (Eq a) => a -> Tree a -> Bool
occurs value (Leaf value') = value == value'
occurs value (Node left value' right) = value == value' ||
                                        occurs value left ||
                                        occurs value right 
