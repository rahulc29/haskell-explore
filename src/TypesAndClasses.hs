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
