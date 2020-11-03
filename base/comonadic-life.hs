module Main where

-- Source: https://herebeseaswines.net/essays/2020-10-22-conways-game-of-life-in-haskell

import Control.Comonad (Comonad (extract, duplicate, extend))
import Control.Concurrent (threadDelay)

data ListZipper a = LZ [a] a [a]

left :: ListZipper a -> Maybe (ListZipper a)
left (LZ (l:ls) x rs) = Just $ LZ ls l (x:rs)
left _ = Nothing

right :: ListZipper a -> Maybe (ListZipper a)
right (LZ ls x (r:rs)) = Just $ LZ (x:ls) r rs
right _ = Nothing

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

flatten :: ListZipper a -> Int -> [a]
flatten (LZ ls x rs) n = reverse (take n ls) <> [x] <> take n rs

instance Functor ListZipper where
	fmap f (LZ ls x rs) = LZ (f <$> ls) (f x) (f <$> rs)

move :: (z a -> z a) -> (z a -> z a) -> z a -> ListZipper (z a)
move a b z = LZ (iterate' a z) z (iterate' b z)

iterate' :: (a -> a) -> a -> [a]
iterate' f = tail . iterate f

instance Comonad ListZipper where
	duplicate z = move (maybe z id . left) (maybe z id . right) z
	extract (LZ _ x _) = x

data Z a = Z (ListZipper (ListZipper a))

data Direction = North | South | East | West

go :: Direction -> Z a -> Z a
go North (Z z) = Z . maybe z id $ left z
go South (Z z) = Z . maybe z id $ right z
go East (Z z) = Z $ (\x -> maybe x id $ left x) <$> z
go West (Z z) = Z $ (\x -> maybe x id $ right x) <$> z

instance Functor Z where
	fmap f (Z z) = Z (fmap (fmap f) z)

horizontal :: Z a -> ListZipper (Z a)
horizontal = move (go East) (go West)

vertical :: Z a -> ListZipper (Z a)
vertical = move (go North) (go South)

instance Comonad Z where
	extract (Z z) = extract $ extract z
	duplicate z = Z $ horizontal <$> vertical z

neighbours :: [Z a -> Z a]
neighbours = horiz <> vert <> ((.) <$> horiz <*> vert) where
	horiz = [go East, go West]
	vert  = [go North, go South]

data Status = Dead | Alive deriving Eq

aliveNeighbours :: Z Status -> Int
aliveNeighbours z = card $ extract . ($ z) <$> neighbours

card :: [Status] -> Int
card = length . filter (== Alive)

rule :: Z Status -> Status
rule z = case aliveNeighbours z of
	2 -> extract z
	3 -> Alive
	_ -> Dead

evolve :: Z Status -> Z Status
evolve = extend rule

display :: Z Status -> String
display (Z z) = unlines $ line <$> flatten z 5 where

	line :: ListZipper Status -> String
	line z = cell <$> flatten z 5

	cell Alive  = '*'
	cell Dead = '_'

glider :: Z Status
glider = Z $ LZ (repeat noone) noone rs where

	rs = [line [Dead, Alive, Dead], line [Dead, Dead, Alive], line [Alive, Alive, Alive]] <> repeat noone
	free = repeat Dead
	noone = LZ free Dead free
	line l = LZ free Dead $ l <> free

gameloop field = do
	putStr "\ESC[2J"
	putStr $ display field
	threadDelay 100000
	gameloop $ evolve field

main = gameloop glider
