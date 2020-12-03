> {-# OPTIONS_HADDOCK show-extensions #-}

> {-|
> Module     : CKY
> Copyright  : (c) 2020 Dakotah Lambert
>
> Just a basic CKY parser
> -}

> module CKY where

> import Data.Bifunctor
> import Data.Set (Set)
> import qualified Data.Set as Set

> import Cartesian


Grammar
=======

A grammar is a set of rules in Chomsky Normal Form.
That is, a nonterminal rewrites either to two nonterminals
or to a single terminal.

> type Grammar f n t = Set (f (Rule n t))
> data Rule n t = Rule n (n,n)
>               | LI n t
>                 deriving (Eq, Ord, Read, Show)
> parent :: Rule n t -> n
> parent (Rule n _) = n
> parent (LI n _) = n

> addRule :: (Functor f, Ord (f (Rule n t))) =>
>            f (n, (n, n)) -> Grammar f n t -> Grammar f n t
> addRule r g = Set.insert (fmap (uncurry Rule) r) g

> addLI :: (Functor f, Ord (f (Rule n t))) =>
>          f (n, t) -> Grammar f n t -> Grammar f n t
> addLI x g = Set.insert (fmap (uncurry LI) x) g

> emptyG :: Grammar f n t
> emptyG = Set.empty

A tree is formed by recursively expanding rules.

> data Tree n t
>     = Tree n (Tree n t) (Tree n t)
>     | Leaf n t
>     deriving (Eq, Ord, Read, Show)
> type Forest f n t = [f (Tree n t)]
> root :: Tree n t -> n
> root (Tree n _ _) = n
> root (Leaf n _) = n


Optional Weighting
==================

The reason for using an arbitrary functor within the grammar
is to allow parsing either with or without probabilities.
We might use an Identity functor, which bears no additional information,
or we might use a Weighted type, augmenting each rule with a weight.
Because we care about the values contained by the functors,
we'll also define an Extractable class.

> class Extractable f where
>     extract :: f x -> x

> data Identity x = Identity x deriving (Eq, Ord, Read, Show)
> instance Extractable Identity where
>     extract (Identity x) = x
> instance Functor Identity where
>     fmap f (Identity x) = Identity (f x)
> instance Applicative Identity where
>     pure x = Identity x
>     Identity f <*> Identity x = Identity (f x)

> data Weighted w x = Weighted w x deriving (Eq, Ord, Read, Show)
> getWeight :: Weighted w x -> w
> getWeight (Weighted w _) = w
> instance Extractable (Weighted w) where
>     extract (Weighted w x) = x
> instance Functor (Weighted w) where
>     fmap f (Weighted w x) = Weighted w (f x)
> instance Bifunctor Weighted where
>     bimap f g (Weighted w x) = Weighted (f w) (g x)
> instance Num w => Applicative (Weighted w) where
>     pure x = Weighted 1 x
>     Weighted w1 f <*> Weighted w2 x = Weighted (w1 * w2) (f x)

Arbitrary weights are not conducive to simple processing;
here we define an idempotent function
that translates these weights to probabilities:

> probabilify :: (Ord a, Ord b, Ord w, Fractional w) =>
>                Grammar (Weighted w) a b -> Grammar (Weighted w) a b
> probabilify g
>     | Set.null g = g
>     | otherwise = Set.union qs
>                   $ Set.map (first (/ s)) ps
>     where p = parent . extract $ Set.findMin g
>           (ps, qs) = Set.partition ((== p) . parent . extract) g
>           s = sum . map getWeight $ Set.toList ps

 
CKY Parsing
===========

To fill in the main diagonal of a CKY parse table, we use leafen
to translate an LI into a basic tree.

> leafen :: (Extractable f, Functor f, Eq t) =>
>           Grammar f n t -> t -> Forest f n t
> leafen g t = map (fmap mktree) . filter (h . extract) $ Set.toList g
>     where h (LI _ x) = x == t
>           h _ = False
>           mktree r = Leaf (parent r) t

Then subsequent steps will result from merging previous trees.

> merge :: (Applicative f, Extractable f, Eq n) =>
>          Grammar f n t -> f (Tree n t) -> f (Tree n t) -> Forest f n t
> merge g wtl wtr = map mktree . filter (h . extract) $ Set.toList g
>     where tl = extract wtl
>           tr = extract wtr
>           h (Rule _ x) = x == (root tl, root tr)
>           h _ = False
>           mktree r = fmap (Tree . parent) r <*> wtl <*> wtr

Each cell of the table may contain multiple valid parses,
so to combine two cells we must consider each pair of subtrees.
This makes use of the cartesian product.

> combineCells :: (Applicative f, Extractable f, Eq n) =>
>                 Grammar f n t -> Forest f n t -> Forest f n t
>              -> Forest f n t
> combineCells g p q = concatMap (uncurry (merge g)) $ cartesian p q

The online approach to CKY parsing adds cells one column at a time.
Each terminal encountered adds a new column.
Eventually, the final parses will be in the last cell calculated.
Only those that end in the right nonterminals are "valid".

 Tables, drawn for my convenience:
 [a] -> [b ]    -> [c ]
        [ab][a]    [bc][b ]
                   [ac][ab][a ]
 
> ckyExtend :: (Applicative f, Extractable f, Eq n, Eq t) =>
>              Grammar f n t -> t -> [[Forest f n t]] -> [[Forest f n t]]
> ckyExtend g a xs = zipWith (:) nexts ([] : xs)
>     where nexts = leafen g a : map (concat . f) [0..]
>           f n = zipWith (combineCells g) (take (n + 1) nexts) (xs!!n)

> ckyParse :: (Applicative f, Extractable f, Ord n, Eq t) =>
>             Grammar f n t -> Set n -> [t] -> Forest f n t
> ckyParse g term = filter (flip Set.member term . root . extract)
>                   . f . foldr (ckyExtend g) []
>     where f [] = []
>           f xs = case last xs of
>                    [] -> []
>                    (x:_) -> x
