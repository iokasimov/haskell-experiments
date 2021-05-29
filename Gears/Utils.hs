module Gears.Utils where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import qualified GHC.Int as Base (eqInt)
import qualified Prelude as Base (Int, String, Semigroup ((<>)), Show (show), min, max, take, reverse, (+), (-))

import Gears.Instances ()

stack_to_list :: [a] -> List a -> [a]
stack_to_list r (TU (Just (Construct x next))) = stack_to_list (x : r) $ TU next
stack_to_list r (TU Nothing) = r

nonempty_stack_to_list :: [a] -> Nonempty List a -> [a]
nonempty_stack_to_list r (Construct x Nothing) = x : r
nonempty_stack_to_list r (Construct x (Just next)) = x : nonempty_stack_to_list r next

stream_to_list :: Stream ~> []
stream_to_list (Construct x (Identity next)) = x : stream_to_list next

zipper_list_to_list :: Zipper List (Left ::: Right) a -> [a]
zipper_list_to_list (Tap x (T_U (bs :*: fs))) = Base.reverse (stack_to_list [] bs) Base.<> [x] Base.<> stack_to_list [] fs

show_zipper_list :: Base.Show a => Zipper List (Left ::: Right) a -> Base.String
show_zipper_list (Tap x (T_U (bs :*: fs))) = Base.show bs Base.<> " =: " Base.<> Base.show x Base.<> " := " Base.<> Base.show fs

-- nat :: Base.Int -> Numerator
-- nat n = n == 0 ? Zero $ Numerator . nat $ n Base.- 1

int :: Numerator -> Base.Int
int (Numerator n) = denum_int n
int Zero = 0

denum_int :: Denumerator -> Base.Int
denum_int (Denumerator n) = 1 + denum_int n
denum_int One = 1

take_n_stream :: Base.Int -> Stream ~> []
take_n_stream n = Base.take n . stream_to_list
