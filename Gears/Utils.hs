module Gears.Utils where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import qualified GHC.Int as Base (eqInt)
import qualified Prelude as Base (Int, Semigroup ((<>)), Show (show), min, max, take, (+), (-))

import Gears.Instances ()

stack_to_list :: [a] -> Stack a -> [a]
stack_to_list r (TU (Just (Construct x next))) = stack_to_list (x : r) $ TU next
stack_to_list r (TU Nothing) = r

nonempty_stack_to_list :: [a] -> Nonempty Stack a -> [a]
nonempty_stack_to_list r (Construct x Nothing) = x : r
nonempty_stack_to_list r (Construct x (Just next)) = x : nonempty_stack_to_list r next

stream_to_list :: Stream ~> []
stream_to_list (Construct x (Identity next)) = x : stream_to_list next

-- nat :: Base.Int -> Numerator
-- nat n = n == 0 ? Zero $ Numerator . nat $ n Base.- 1

int :: Numerator -> Base.Int
int (Numerator n) = 1 + denum_int n
int Zero = 0

denum_int :: Denumerator -> Base.Int
denum_int (Denumerator n) = 1 + denum_int n
denum_int One = 1

take_n_stream :: Base.Int -> Stream ~> []
take_n_stream n = Base.take n . stream_to_list
