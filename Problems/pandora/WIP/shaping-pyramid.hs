import "base" Data.Int (Int)
import "base" Data.List ((++))
import "base" System.IO (print)
import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances ()
import Gears.Utils (show_zipper_list)

data Hill = Uphill | Downhill

validate :: Zipper List Int -> Maybe (Zipper List Int)
validate z@(Tap x (T_U (bs :*: fs))) = check_sides $> z where

	check_sides :: Maybe ()
	check_sides = let check_side side = run (side ->> uphill) 0 in
		void $ check_side (item @Push x bs) -- *> check_side (Reverse fs)

uphill :: Int -> State Int :> Maybe := Int
uphill now = now == 0 ? point 0 $ current @Int >>= \before ->
	now - before == 1 ? replace @Int now $ nothing

focused :: (Maybe <:.> Zipper List := Int) -> Int
focused = resolve @(Zipper List Int) extract 0 . run

to_zipper :: Nonempty List ~> Zipper List
to_zipper xs = Tap (extract xs) $ twosome / unite (deconstruct xs) / empty

wrong_example, valid_example :: Nonempty List Int
wrong_example = item @Push 0 $ item @Push 1 $ item @Push 3
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0
valid_example = item @Push 0 $ item @Push 1 $ item @Push 2
	$ item @Push 3 $ item @Push 2 $ item @Push 1 $ point 0

show_moves list = do
	let start = to_zipper list
	let z = show_zipper_list start
	print $ "0) " ++ z
	let Just z1 = run (rotate @Left start)
	print $ "1) " ++ show_zipper_list z1
	let Just z2 = run (rotate @Left start) >>= run . rotate @Left
	print $ "2) " ++ show_zipper_list z2
	let Just z3 = run (rotate @Left start) >>= run . rotate @Left >>= run . rotate @Left
	print $ "3) " ++ show_zipper_list z3
	point z3

main = void $ do
	print "------------------------------------------------------"
	Tap x (T_U (bs :*: fs)) <- show_moves valid_example
	print "------------------------------------------------------"
	print x
	print bs
	print fs
	print "------------------------------------------------------"
	let start = to_zipper valid_example
	(start =>> validate) ->> print . (<$>) show_zipper_list
