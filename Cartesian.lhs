> {-# OPTIONS_HADDOCK show-extensions #-}

> {-|
> Module     : Cartesian
> Copyright  : (c) 2020 Dakotah Lambert
>
> Efficient cartesian products over possibly-infinite lists
> using diagonalization
> -}

> module Cartesian (cartesian, diagonalize) where

> cartesian :: [a] -> [b] -> [(a, b)]
> cartesian = curry (concat . diagonalize . uncurry basicProduct)

> diagonalize :: [[a]] -> [[a]]
> diagonalize = drop 1 . f []
>     where f crows rest
>               = heads
>                 : case rest of
>                     []   -> transpose crows'
>                     r:rs -> f (r:crows') rs
>               where ~(heads, crows') = g crows
>                     g [] = ([], [])
>                     g (x:xs) = let ~(hs, ts) = g xs
>                                in case x of
>                                     [] -> (hs, ts)
>                                     (y:[]) -> (y : hs, ts)
>                                     (y:ys) -> (y : hs, ys : ts)

> basicProduct :: [a] -> [b] -> [[(a, b)]]
> basicProduct as bs = map (\a -> map ((,) a) bs) as

> transpose :: [[a]] -> [[a]]
> transpose [] = []
> transpose xs = concatMap (take 1) xs
>                : transpose (filter (not . null) $ map (drop 1) xs)
