import "base" Data.Int (Int)
import "base" Data.List ((++))
import "base" System.IO (print)
import "base" Text.Show (show)
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances ()
import Gears.Utils (show_zipper_list)

validate :: Zipper List Int -> Maybe (Zipper List Int)
validate zipper = (|- pyramid) $ side <-> side $ run $ lower zipper where

	pyramid :: Maybe (List Int) -> Maybe (List Int) -> Maybe (Zipper List Int)
	pyramid ls rs = Tap (extract zipper) <$> (twosome <$> ls <*> rs)

	side :: List Int -> Maybe (List Int)
	side xs = extract <$> run (xs ->> slide) (extract zipper)

	slide :: Int -> State Int :> Maybe := Int
	slide now = current @Int >>= \before ->
		before - now == 1 ? replace @Int now $ nothing

type Enumeration = Comprehension Maybe

reshape :: Zipper List Int -> Enumeration :. Zipper List := Int
reshape zipper = join . join $ Comprehension . sieve . (=>> validate)
	<$$> Comprehension . zipper_list_to_list <$> chip where

	chip :: Enumeration :. Zipper List :. Zipper List := Int
	chip = duplicate zipper ->> point . over (focus @Head) decrement

	sieve :: forall a t . Traversable t => t (Maybe a) -> List a
	sieve possible = attached . run @(State _) % empty
		$ possible ->> resolve @a (void . modify @(List a) . item @Push) (point ())

decrement :: (Quasiring a, Group a) => a -> a
decrement x = x - one

-- attempt :: Zipper List Int -> Enumeration :. Zipper List := Int
-- attempt bricks = let result = reshape bricks in result == empty ? join (reshape <$> result) $ result

nonempty_list_to_zipper_list :: Nonempty List ~> Zipper List
nonempty_list_to_zipper_list xs = Tap (extract xs) $ twosome / unite (deconstruct xs) / empty

zipper_list_to_list :: Zipper List ~> List
zipper_list_to_list (Tap x (T_U (bs :*: fs))) =
	attached . run @(State _) % item @Push x bs
		$ fs ->> modify @(List _) . item @Push

wrong_example, valid_example :: Nonempty List Int
wrong_example = item @Push 0 $ item @Push 1 $ item @Push 3
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0
valid_example = item @Push 0 $ item @Push 1 $ item @Push 2
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0

main = void $ do
	print wrong_example
	let start = nonempty_list_to_zipper_list wrong_example
	print "..................."
	-- attempt start ->> print
	reshape start ->> print . zipper_list_to_list
