import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern

import GHC.Int (Int, eqInt)
import Prelude (Char, Show (show), print, reverse, (-), (<>))
import Control.Concurrent (threadDelay)

type Status = Boolean

type Field = Zipper Stream Status

type Neighbours = Status :*: Status :*: Status

start :: Field
start = let desert = repeat False
	in Tap True . TU $ desert :^: desert

rule50 :: Neighbours -> Status
rule50 (True :*: True :*: True) = False
rule50 (True :*: True :*: False) = False
rule50 (True :*: False :*: True) = True
rule50 (True :*: False :*: False) = True
rule50 (False :*: True :*: True) = False
rule50 (False :*: True :*: False) = False
rule50 (False :*: False :*: True) = True
rule50 (False :*: False :*: False) = False

neighbourhood :: Field -> Neighbours
neighbourhood z = extract (sub @Left ^. z) :*: extract z :*: extract (sub @Right ^. z)

display :: Natural -> Field -> [Status]
display n (Tap x (TU (bs :^: fs))) = take_n n bs [] <> [x] <> reverse (take_n n fs []) where

	take_n :: Natural -> Stream a -> [a] -> [a]
	take_n (Natural n) (Construct x (Identity next)) r = take_n n next $ x : r
	take_n Zero _ r = r

instance Show Boolean where
	show True = "*"
	show False = " "

instance Setoid Int where
	x == y = if eqInt x y then True else False

natural :: Int -> Natural
natural n = n == 0 ? Zero $ Natural . natural $ n - 1

instance Monotonic (Construction Identity a) a where
	bypass f r ~(Construct x (Identity xs)) = f x $ bypass f r xs

lifecycle act being = do
	threadDelay 1000000
	print $ display (natural 10) being
	lifecycle act $ being =>> act

main = lifecycle (rule50 . neighbourhood) start
