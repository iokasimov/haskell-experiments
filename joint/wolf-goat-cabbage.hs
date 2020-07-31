import "base" Data.Functor (($>))
import "base" Data.List (delete)
import "lens" Control.Lens (Lens', view, _1, _2, (%~))
import "joint" Control.Joint (Liftable, Stateful, State, lift, current, modify, run, type (:>), type (:=))

data Character = Wolf | Goat | Cabbage deriving Eq

instance Show Character where
	show Wolf = "🐺"
	show Goat = "🐐"
	show Cabbage = "🥬"

class Survivable a where
	survive :: a -> a -> Ordering

instance Survivable Character where
	survive Wolf Goat = GT
	survive Goat Wolf = LT
	survive Goat Cabbage = GT
	survive Cabbage Goat = LT
	survive _ _ = EQ

data Direction = Back | Forward

type River a = ([a], [a])

type Iterable = Liftable []

step :: forall a . (Eq a, Survivable a) => Direction -> State (River a) :> [] := Maybe a
step direction = bank >>= next >>= transport where

	bank :: (Functor t, Stateful (River a) t) => t [a]
	bank = view (source direction) <$> current

	next :: (Survivable a, Iterable t) => [a] -> t (Maybe a)
	next xs = lift $ filter valid $ Nothing : (Just <$> xs) where

		valid :: Maybe a -> Bool
		valid Nothing = and $ coexist <$> xs <*> xs
		valid (Just x) = and $ coexist <$> delete x xs <*> delete x xs

		coexist :: Survivable a => a -> a -> Bool
		coexist x y = survive x y == EQ

	transport :: (Eq a, Applicative t, Stateful (River a) t) => Maybe a -> t (Maybe a)
	transport Nothing = pure Nothing
	transport (Just x) = modify @(River a) (leave . land) $> Just x where

		leave, land :: River a -> River a
		leave = source direction %~ delete x
		land = target direction %~ (x :)

	source, target :: Direction -> Lens' (River a) [a]
	source Back = _2
	source Forward = _1
	target Back = _1
	target Forward = _2

start :: River Character
start = ([Goat, Wolf, Cabbage], [])

route :: [Direction]
route = iterate alter Forward where

	alter :: Direction -> Direction
	alter Back = Forward
	alter Forward = Back

main = traverse print $ filter ((==) [] . fst . fst)
	$ run (traverse step $ take 7 route) start
