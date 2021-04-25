import "base" Data.Int (Int)
import "base" System.IO (print)
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances ()

type Chippable a = (Setoid a, Quasiring a, Group a)

type Shape = Zipper List

type Phase a = Shape :. Shape := a

-- Fails if a given shape has pyramid shape - it's considered as found solution
validate :: forall a . Chippable a => Shape a -> Conclusion (Shape a) (Shape a)
validate z = resolve @(Shape a) failure (point z) . (|- pyramid) . (side <-> side) . run $ lower z where

	pyramid :: Maybe (List a) -> Maybe (List a) -> Maybe (Shape a)
	pyramid ls rs = Tap (extract z) <$> (twosome <$> ls <*> rs)

	side :: List a -> Maybe (List a)
	side xs = extract <$> run (xs ->> slide) (extract z)

	slide :: Chippable a => a -> State a :> Maybe := a
	slide now = let decide before = before - now == one ? replace now $ nothing in current >>= decide

-- Explore variants by removing brick from each column as a new variant
explore :: forall a . Chippable a => Shape a -> Conclusion (Shape a) (Phase a)
explore bricks = chipped <$> sequence (bricks =>> validate) where

	-- Remove a brick from focused column
	chipped :: Phase a -> Phase a
	chipped shifted = over (focus @Head) decrement <$> shifted

	decrement :: a -> a
	decrement x = x == zero ? zero $ x - one

-- 0 1 4 3 2 1 0
not_valid_example :: Nonempty List Int
not_valid_example = item @Push 0 $ item @Push 1 $ item @Push 4
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0

valid_example :: Nonempty List Int
valid_example = item @Push 0 $ item @Push 1 $ item @Push 2
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0

main = void $ do
	let not_valid = into @(Zipper List) not_valid_example
	print $ explore not_valid >>= (->> explore) >>= (->>> explore)
