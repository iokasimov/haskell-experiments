import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, print)

import Gears.Instances ()
import Gears.Utils (int, stack_to_list, nonempty_stack_to_list)

--------------------------------------------------------------------------------

-- join $ increase (extract seq) . extract <$> deconstruct seq

try :: Nonempty Stack Int -> Nonempty Stack (Maybe (Nonempty Stack (Stack Int)))
try seq = seq =>> connect

connect :: Nonempty Stack Int -> Maybe (Construction Maybe (Stack Int))
connect subseq = increase (extract subseq) <$$> deconstruct subseq where

	increase :: Int -> Int -> Stack Int
	increase x y = x < y ? (empty & insert x & insert y) $ unite Nothing

example :: Nonempty Stack Int
example = insert 3 $ insert 1 $ insert 8 $ insert 2 $ point 5

main = void $ try example ->>> print . comap (stack_to_list []) . nonempty_stack_to_list []
