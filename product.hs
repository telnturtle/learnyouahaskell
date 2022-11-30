product' :: [a] -> [b] -> [(a,b)]
product' []  _bs     = []
product' _as []      = []
product' (a:as) _bs  = (aux a _bs) ++ (product' as _bs)
    where   aux :: a -> [b] -> [(a,b)]
            aux a = zip $ repeat a

product'' :: [a] -> [b] -> [(a,b)]
product'' as bs = [ (a,b) | a <- as, b <- bs ]