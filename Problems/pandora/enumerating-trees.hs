import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)

import Gears.Instances ()
import Gears.Utils (int)

zipper_to_binary_degenerated :: Zipper Stack ~> Nonempty Binary
zipper_to_binary_degenerated (Tap x (TU (bs :^: fs))) = Construct x $ branches
	(hoist (branch Left) <$> run bs) (hoist (branch Right) <$> run fs) where

	branch :: a |-> Wye -> Maybe a -> Wye a
	branch f (Just x) = f x
	branch _ Nothing = End

--------------------------------------------------------------------------------

example :: Nonempty Stack Int
example = insert 1 $ insert 2 $ point 3

example_zipper :: Zipper Stack Int
example_zipper = Tap 1 . TU $ empty :^: unite (deconstruct example)

main = void . print . extract $ example_zipper =>> int . cardinality
