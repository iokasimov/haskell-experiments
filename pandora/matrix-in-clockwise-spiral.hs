import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Prelude (Int, Show, print)
import qualified Prelude as P

type Vector v a = Monotonic v a

vector_total_sum :: Vector v Int => v -> Int
vector_total_sum = iterate (P.+) (0 :: Int)

type Matrix m v a = (Monotonic m v, Vector v a)

--------------------------------------------------------------------------------

-- [[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15],[16,17,18,19,20]]
-- ==> 1 2 3 4 5 10 15 20 19 18 17 16 11 6 7 8 9 14 13 12

type M54 a = V5 a :*: V5 a :*: V5 a :*: V5 a

matrix_example :: M54 Int
matrix_example = (1 :*: 2 :*: 3 :*: 4 :*: 5)
	:*: (6 :*: 7 :*: 8 :*: 9 :*: 10)
	:*: (11 :*: 12 :*: 13 :*: 14 :*: 15)
	:*: (16 :*: 17 :*: 18 :*: 19 :*: 20)

type V5 a = a :*: a :*: a :*: a :*: a

vector_example :: V5 Int
vector_example = 1 :*: 2 :*: 3 :*: 4 :*: 5

deriving instance (Show a, Show b) => Show (a :*: b)
instance Semigroup Int where (+) = (P.+)

last :: V5 Int -> Int
last = iterate (\x r -> x) P.undefined

main = do
	print $ vector_total_sum vector_example
	print $ iterate (\x r -> r P.+ vector_total_sum @(V5 Int) x) (0 :: Int) matrix_example
	print ((1 :*: 2 :*: 3 :*: 4 :*: 5) + (6 :*: 7 :*: 8 :*: 9 :*: 10) :: V5 Int)
	print $ last vector_example
