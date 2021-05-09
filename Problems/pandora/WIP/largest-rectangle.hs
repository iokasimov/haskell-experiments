import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" System.IO (print)

import Gears.Instances ()

take_while :: Int -> Int -> State Int :> Conclusion Int := ()
take_while pattern bar = bar >= pattern ? void # modify @Int (+ pattern) $ current @Int >>= failure

rectangular :: Zipper List Int -> Int
rectangular (Tap x (T_U (ls :*: rs))) = side ls + x + side rs where

	side xs = conclusion identity attached $
		run (xs ->> take_while x) zero

example_list :: Nonempty List Int
example_list = item @Push 1 $ item @Push 3 $ item @Push 2 $ item @Push 5 $ point 1

main = void $ do
	print $ into @(Zipper List) example_list =>> rectangular
