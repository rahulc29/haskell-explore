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

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int 
nat2int Zero = 0 
nat2int (Succ x) = 1 + nat2int x 

int2nat :: Int -> Nat
int2nat x | x < 0 = undefined
          | x == 0 = Zero 
          | otherwise = Succ(int2nat (x - 1))

add :: Nat -> Nat -> Nat 
add Zero n = n 
add (Succ m) n = Succ (add m n)

mult Zero n = Zero
mult (Succ Zero) n = n
mult (Succ m) n = add (mult m n) n

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: (Ord a) => a -> Tree a -> Bool 
occurs value (Leaf value') = value == value' 
occurs value (Node l v r) = occurs' value l v r (compare value v)
                            where occurs' value l v r LT = occurs value l 
                                  occurs' value l v r GT = occurs value r 
                                  occurs' value l v r EQ = True