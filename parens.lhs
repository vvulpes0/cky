> module Main where

> import Data.Char (isSpace)
> import Data.Set (Set)
> import qualified Data.Set as Set

> import CKY

> main = interact (showForest . ckyParse g (Set.singleton S) . tokenize)

> data Node = S | T | L | R
>             deriving (Eq, Ord, Read, Show)

> li x y = Identity (x, y)
> rule x y = Identity (x, y)

> g = addRule (rule S (L, R))
>     . addRule (rule S (S, S))
>     . addRule (rule S (L, T))
>     . addRule (rule T (S, R))
>     . addLI (li L "(")
>     . addLI (li R ")")
>     $ emptyG

> tokenize :: String -> [String]
> tokenize = map pure

> showForest :: (Show n, Show t, Extractable f) => Forest f n t -> String
> showForest f = unlines [ "graph {"
>                        , showForest' f 0 ++ "}"
>                        ]

> showForest' :: (Show n, Show t, Extractable f) =>
>                Forest f n t -> Int -> String
> showForest' [] _ = ""
> showForest' (x:xs) n = uncurry (++) . fmap (showForest' xs)
>                        $ showTree' x n

> showTree' :: (Show n, Show t, Extractable f) =>
>              f (Tree n t) -> Int -> (String, Int)
> showTree' t n = uncurry f $ showTree (extract t) n
>     where f a b = (unlines $ concat
>                    [ ["subgraph cluster_" ++ show n ++ " {"]
>                    , a
>                    , ["}"]
>                    ]
>                   , b)

> showTree :: (Show n, Show t) => Tree n t -> Int -> ([String], Int)
> showTree t n = case t of
>                  Leaf p x ->  ([ lbl n p
>                                , lbl (n + 1) x
>                                , connect n (n + 1)
>                                ]
>                               , n + 2)
>                  Tree p a b -> let (left, after) = showTree a (n + 1)
>                                    (right, end) = showTree b after
>                                in (concat
>                                    [ [lbl n p
>                                      , connect n (n + 1)
>                                      , connect n after
>                                      ]
>                                    , left
>                                    , right
>                                    ]
>                                   , end + 1)
>     where lbl n' x = show n' ++ " [label=\""
>                      ++ (filter (/= '"') . show) x ++ "\"];"
>           connect a b = show a ++ " -- " ++ show b ++ ";"
