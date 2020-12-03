> module Main where

> import Data.Char (isSpace)
> import Data.Set (Set)
> import qualified Data.Set as Set

> import CKY

> main = interact (showForest . ckyParse g (Set.singleton S) . tokenize)

> data Node = S | D | DP | NP | P | PP | V | VP
>             deriving (Eq, Ord, Read, Show)

> li x y = Identity (x, y)
> rule x y = Identity (x, y)

> g = addRule (rule S (DP, VP))
>     . addRule (rule DP (DP, PP))
>     . addRule (rule DP (D, NP))
>     . addRule (rule PP (P, DP))
>     . addRule (rule VP (VP, PP))
>     . addRule (rule VP (V, DP))
>     . addLI (li D "the")
>     . addLI (li NP "binoculars")
>     . addLI (li NP "crow")
>     . addLI (li NP "girl")
>     . addLI (li P "with")
>     . addLI (li V "saw")
>     $ emptyG

> tokenize :: String -> [String]
> tokenize xs
>     | all isSpace xs = []
>     | otherwise = uncurry (:) . fmap tokenize
>                   . break isSpace $ dropWhile isSpace xs

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
