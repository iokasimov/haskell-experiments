module Main where

-- Source: https://kwannoel.xyz/22050263.html

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving Show

type Loc a = (Tree a, Context a)

data Context a = Top | L (Context a) (Tree a) | R (Context a) (Tree a) deriving Show

left :: Loc a -> Loc a
left (Branch l r, c) = (l, L c r)

right :: Loc a -> Loc a
right (Branch l r, c) = (r, R c l)

top :: Tree a -> Loc a
top t = (t, Top)

up :: Loc a -> Loc a
up (t, L c r) = (Branch t r, c)
up (t, R c l) = (Branch l t, c)

upmost :: Loc a -> Loc a
upmost l@(t, Top) = l
upmost l = upmost (up l)

modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t, c) f = (f t, c)

--------------------------------------------------------------------------------

example_tree :: Tree Int
example_tree = Branch
    (Branch (Leaf 1) (Leaf 2))
    (Branch (Leaf 3) (Leaf 4))

example_loc :: Loc Int
example_loc = (Leaf 2, R (L Top (Branch (Leaf 3) (Leaf 4))) (Leaf 1))

main = do
    -- print example_tree
    print example_loc
    print $ up example_loc
    print $ left $ up example_loc
