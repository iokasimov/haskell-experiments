import "base" System.IO (print)
import "base" Text.Show (Show (show))
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Utils (take_n_stream)

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

type River = List Character :*: List Character

type Enumeration = Comprehension Maybe

step :: Boolean -> State River :> Enumeration := Maybe Character
step way = adapt bank >>= adapt . choice >>= adapt . (->> transport) where

	bank :: State River := List Character
	bank = view source <$> current

	choice :: List Character -> Enumeration :. Maybe := Character
	choice xs = Comprehension $ filter @All (lunchtime >$< null) boats where

		lunchtime :: Maybe Character -> Maybe :. Enumeration := ()
		lunchtime x = sequence $ survive <$> selection x <*> selection x

		selection :: Maybe Character -> Enumeration Character
		selection = Comprehension . resolve @Character (delete @First % xs) xs

		boats :: List :. Maybe := Character
		boats = item @Push Nothing $ Just <$> xs

	transport :: Character :=> State River
	transport being = source ~<> delete @First being *> target ~<> item @Push being $> being

	source, target :: River :-. List Character
	source = way ? sub @Right $ sub @Left
	target = way ? sub @Left $ sub @Right

route :: Stream Boolean
route = point . bool True False .-+ False

--------------------------------------------------------------------------------

start :: River
start = characters :*: empty where

	characters :: List Character
	characters = item @Push Wolf $ item @Push Goat $ item @Push Cabbage $ empty

solution :: List [Maybe Character]
solution = extract <$> filter @All moved result where

	result :: List (River :*: [Maybe Character])
	result = run . run % start $ take_n_stream 7 route ->> step

	moved :: Predicate (River :*: [Maybe Character])
	moved = null >&< view (sub @Left . sub @Left)

main = solution ->> print
