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

instance Show Character where
	show Wolf = "🐺"
	show Goat = "🐐"
	show Cabbage = "🥬"

survive :: Character -> Character -> Maybe ()
survive Wolf Goat = Nothing
survive Goat Wolf = Nothing
survive Goat Cabbage = Nothing
survive Cabbage Goat = Nothing
survive _ _ = Just ()

type River = Delta :. Stack := Character

type Enumeration = Comprehension Maybe

step :: Boolean -> State River :> Enumeration := Maybe Character
step way = adapt bank >>=:> choice >>=:> transport where

	bank :: State River := Stack Character
	bank = view (source way) <$> current

	choice :: Stack Character -> Enumeration :. Maybe := Character
	choice xs = Comprehension . filter (not $ lunchtime >$< null) $ boats where

		lunchtime :: Maybe Character -> Maybe :. Enumeration := ()
		lunchtime x = sequence $ survive <$> selection x <*> selection x

		selection :: Maybe Character -> Enumeration Character
		selection = Comprehension . resolve @Character (view (sub @(Delete First)) xs) xs

		boats :: Stack :. Maybe := Character
		boats = insert Nothing $ Just <$> xs

	transport :: Maybe Character |-> State River
	transport Nothing = point Nothing
	transport (Just x) = modify @River (leave . land) $> Just x where

		leave, land :: River -> River
		leave = source way %~ (view (sub @(Delete First)) % x)
		land = target way %~ insert x

	source, target :: Boolean -> River :-. Stack Character
	source = represent . bool True False
	target = represent

--------------------------------------------------------------------------------

start :: River
start = characters :^: empty where

	characters :: Stack Character
	characters = insert Wolf $ insert Goat $ insert Cabbage $ empty

solution :: Stack [Maybe Character]
solution = extract <$> filter moved result where

	path :: [Boolean]
	path = take_n_stream 7 route

	route :: Stream Boolean
	route = point . bool True False .-+ zero

	result :: Stack (River :*: [Maybe Character])
	result = run . run % start $ path ->> step

	moved :: Predicate (River :*: [Maybe Character])
	moved = null >&< view (sub @Left |> sub @Left)

main = solution ->> print
