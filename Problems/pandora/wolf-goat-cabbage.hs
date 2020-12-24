import "base" System.IO (print)
import "base" Text.Show (Show (show))
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Utils (take_n_stream, stack_to_list)

data Character = Wolf | Goat | Cabbage

instance Setoid Character where
	Wolf == Wolf = True
	Goat == Goat = True
	Cabbage == Cabbage = True
	_ == _ = False

instance Chain Character where
	Wolf <=> Wolf = Equal
	Wolf <=> Goat = Greater
	Wolf <=> Cabbage = Equal
	Goat <=> Wolf = Less
	Goat <=> Goat = Equal
	Goat <=> Cabbage = Greater
	Cabbage <=> Wolf = Equal
	Cabbage <=> Goat = Less
	Cabbage <=> Cabbage = Equal

class Survivable a where
	survive :: a -> a -> Ordering

instance Survivable Character where
	survive Wolf Goat = Greater
	survive Goat Wolf = Less
	survive Goat Cabbage = Greater
	survive Cabbage Goat = Less
	survive _ _ = Equal

type River = Delta :. Stack := Character

start :: River
start = characters :^: empty where

	characters :: Stack Character
	characters = insert Wolf $ insert Goat $ insert Cabbage $ empty

data Way = Back | Forward

type Enumeration = Comprehension Maybe

route :: Stack Way
route = unite . Just $ point . alter .-+ Forward where

	alter :: Way -> Way
	alter Back = Forward
	alter Forward = Back

step :: Way -> State River :> Enumeration := Maybe Character
step way = bank >>= adapt . next >>= transport where

	bank :: (Covariant t, Stateful River t) => t :. Stack := Character
	bank = view (source way) <$> current

	source, target :: Way -> River :-. Stack Character
	source Back = sub @Right
	source Forward = sub @Left
	target Back = sub @Left
	target Forward = sub @Right

	next :: Stack Character -> Enumeration :. Maybe := Character
	next xs = Comprehension . filter valid . insert Nothing $ Just <$> xs where

		valid :: Predicate :. Maybe := Character
		valid = Predicate $ resolve @() (True !) False . void . sequence . lunchtime

		lunchtime :: Maybe Character -> Enumeration :. Maybe := ()
		lunchtime x = coexist <$> boat x <*> boat x

		boat Nothing = Comprehension xs
		boat (Just x) = Comprehension $ delete x xs

		coexist :: Character -> Character -> Maybe ()
		coexist x y = survive x y == Equal ? Just () $ Nothing

	transport Nothing = point Nothing
	transport (Just x) = modify @River (leave . land) $> Just x where

		leave, land :: River -> River
		leave = source way %~ delete x
		land = target way %~ insert x

--------------------------------------------------------------------------------

instance Show Character where
	show Wolf = "🐺"
	show Goat = "🐐"
	show Cabbage = "🥬"

main = do
	let path = take_n_stream 7 route
	let result = run . run % start $ path ->> step
	let moved = Predicate $ (==) (TU Nothing) . view (sub @Left) . attached
	extract <$> filter moved result ->> print
