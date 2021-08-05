import "base" System.IO (print)
import "base" Text.Show (Show (show))
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Utils (take_n_stream)

import Debug.Trace (traceShow)

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

type River = List <:.:> List := (:*:)

type Enumeration = Comprehension Maybe

step :: Boolean -> State (River Character) :> Enumeration := Maybe Character
step way = adapt bank >>= adapt . choice >>= adapt . (->> transport) where

	bank :: State (River Character) := List Character
	bank = extract . view source <$> current

	choice :: List Character -> Enumeration :. Maybe := Character
	choice xs = (\r -> traceShow (run r) r) $ Comprehension $ filter @All (lunchtime >$< null) boats where

		lunchtime :: Maybe Character -> Maybe :. Enumeration := ()
		lunchtime x = sequence $ survive <$> selection x <*> selection x

		selection :: Maybe Character -> Enumeration Character
		selection = Comprehension . resolve @Character (delete @First % xs) xs

		boats :: List :. Maybe := Character
		boats = item @Push Nothing $ Just <$> xs

	transport :: Character :=> State (River Character)
	transport being = source ~<> (delete @First being <$>) *>
		target ~<> (item @Push being <$>) $> being

	source, target :: Convex Lens (River Character) (List Character)
	source = way ? sub @Right $ sub @Left
	target = way ? sub @Left $ sub @Right

route :: Stream Boolean
route = point . bool True False .-+ False

--------------------------------------------------------------------------------

start :: River Character
start = unite $ characters :*: empty where

	characters :: List Character
	characters = item @Push Wolf $ item @Push Goat $ item @Push Cabbage $ empty

solution :: List [Maybe Character]
solution = extract <$> filter @All moved result where

	result :: List (River Character :*: [Maybe Character])
	result = run . run % start $ take_n_stream 7 route ->> step . (\r -> traceShow r r)

	moved :: Predicate (River Character :*: [Maybe Character])
	moved = null >&< (extract . view (sub @Left . access @(River Character)))

main = solution ->> print
