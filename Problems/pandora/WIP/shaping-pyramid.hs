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
validate z@(Tap x (T_U (bs :*: fs))) =
	let side xs = run (Reverse xs ->> slide) 0 in
	side (item @Push x bs) *> side (item @Push x fs) $> z

slide :: Int -> State Int :> Maybe := Int
slide now = now == 0 ? point 0 $ current @Int >>= \before ->
	now - before == 1 ? replace @Int now $ nothing

fix :: Zipper List Int -> Comprehension Maybe :. Zipper List :. Zipper List := Int
fix z = duplicate z ->> (point . over (focus @Head) (\x -> x - 1))

focused :: (Maybe <:.> Zipper List := Int) -> Int
focused = resolve @(Zipper List Int) extract 0 . run

to_zipper :: Nonempty List ~> Zipper List
to_zipper xs = Tap (extract xs) $ twosome / unite (deconstruct xs) / empty

wrong_example, valid_example :: Nonempty List Int
wrong_example = item @Push 0 $ item @Push 1 $ item @Push 3
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0
valid_example = item @Push 0 $ item @Push 1 $ item @Push 2
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0

main = void $ do
	let start = to_zipper valid_example
	(start =>> validate) ->> print . (<$>) show
