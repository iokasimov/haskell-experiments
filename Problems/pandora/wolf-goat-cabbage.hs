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

survive :: Character -> Character -> Maybe ()
survive Wolf Goat = Nothing
survive Goat Wolf = Nothing
survive Goat Cabbage = Nothing
survive Cabbage Goat = Nothing
survive _ _ = Just ()

type River = Delta :. Stack := Character

start :: River
start = characters :^: empty where

	characters :: Stack Character
	characters = insert Wolf $ insert Goat $ insert Cabbage $ empty

type Enumeration = Comprehension Maybe

route :: Stream Boolean
route = point . invert .-+ zero

step :: Boolean -> State River :> Enumeration := Maybe Character
step way = bank >>=:> choice >>=:> transport where

	bank :: (Covariant t, Stateful River t) => t :. Stack := Character
	bank = view (source way) <$> current

	choice :: Stack Character -> Enumeration :. Maybe := Character
	choice xs = Comprehension . filter valid . insert Nothing $ Just <$> xs where

		valid :: Predicate :. Maybe := Character
		valid = Predicate $ resolve @() (True !) False . void . sequence . lunchtime

		lunchtime :: Maybe Character -> Enumeration :. Maybe := ()
		lunchtime x = survive <$> boat x <*> boat x

		boat Nothing = Comprehension xs
		boat (Just x) = Comprehension $ delete x xs

	transport :: Maybe Character |-> State River
	transport Nothing = point Nothing
	transport (Just x) = modify @River (leave . land) $> Just x where

		leave, land :: River -> River
		leave = source way %~ delete x
		land = target way %~ insert x

	source, target :: Boolean -> River :-. Stack Character
	source = represent . invert
	target = represent

--------------------------------------------------------------------------------

instance Show Character where
	show Wolf = "🐺"
	show Goat = "🐐"
	show Cabbage = "🥬"

main = do
	let path = take_n_stream 7 route
	let result = run . run % start $ path ->> step
	let moved = view (sub @Left) . attached >$< null
	extract <$> filter moved result ->> print
