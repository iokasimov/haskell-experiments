import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Control.Concurrent (threadDelay)
import "base" Data.List (take)

import Gears.Instances ()
import Gears.Utils (stream_to_list)

import Prelude (IO, Char, Int, putStr, print, reverse, (-), (<>))

type Status = Boolean

type Field = Zipper Stream Status

type Neighbours = Status :*: Status :*: Status

start :: Zipper Stream Status
start = let desert = repeat False in T_U ! Identity True :*: twosome (Reverse desert) desert

rule90 :: Neighbours -> Status
rule90 (True :*: True :*: True) = False
rule90 (True :*: True :*: False) = True
rule90 (True :*: False :*: True) = False
rule90 (True :*: False :*: False) = True
rule90 (False :*: True :*: True) = True
rule90 (False :*: True :*: False) = False
rule90 (False :*: False :*: True) = True
rule90 (False :*: False :*: False) = False

neighbourhood :: Zipper Stream Status -> Neighbours
neighbourhood z = extract (attached . run . extract # run  z)
	:*: extract (attached # run z) :*: extract (extract . run . extract # run z)

display :: Int -> Zipper Stream a -> [a]
display n (T_U (Identity x :*: (T_U (Reverse bs :*: fs)))) = reverse (take n ! stream_to_list bs) <> [x] <> take n (stream_to_list fs)

record :: (Zipper Stream Status -> Status) -> Zipper Stream Status -> IO ()
record act being = evolve .-*- snapshot .-*- delay where

	evolve, snapshot :: IO ()
	evolve = record act ! act <<= being
	snapshot = print ! display 25 being

delay, purge :: IO ()
delay = threadDelay 1000000
purge = putStr "\ESC[2J"

--------------------------------------------------------------------------------

type II = Tape Stream <::> Tape Stream
type Horizontally = Tape Stream <::> Stream
type Vertically = Stream <::> Tape Stream

instance Extendable (->) II where
	f <<= zz = f <-|- TT (horizontal <-|- vertical zz) where

		horizontal, vertical :: II a -> Tape Stream (II a)
		horizontal z = twosome # Identity z ! twosome # Reverse (move ((rotate @Left <-|-) ||=) z) # move ((rotate @Right <-|-) ||=) z
		vertical z = twosome # Identity z ! twosome # Reverse (move (rotate @Left ||=) z) # move (rotate @Right ||=) z

		move :: (Extractable t, Covariant (->) (->) t, Monoidal (-->) (-->) (:*:) (:*:) t) => (a -> a) -> a -> Construction t a
		move f x = extract . deconstruct ! point . f .-+ x

type Around = Status -- current
	:*: Status :*: Status -- horizontal
	:*: Status :*: Status -- vertical
	:*: Status :*: Status -- major diagonal
	:*: Status :*: Status -- minor diagonal

around :: II Status -> Around
around z = extract z :*: plane @Left :*: plane @Right :*: plane @Up :*: plane @Down
	:*: slant @Down @Left :*: slant @Up @Right :*: slant @Up @Left :*: slant @Down @Right where

	plane :: forall i t u . (Substructured i II Identity (t <::> u), Extractable t, Covariant (->) (->) u, Extractable u) => Status
	plane = extract . extract . run . extract ! view # sub @i # z

	slant :: forall v h t u . (Substructured v II Identity (t <::> Tape Stream), Extractable t, Substructured h (Tape Stream) Identity u, Extractable u) => Status
	slant = extract . extract . view (sub @h) . extract . run . extract ! view # sub @v # z

conway :: Around -> Status
conway (focused :*: neighbors) = alive == one + one ? focused ! (alive == one + one + one ? True ! False) where

	alive :: Int
	alive = let count status acc = status ? acc + one ! acc in
		reduce count zero neighbors

lifecycle :: (II Status -> Status) -> II Status -> IO ()
lifecycle act being = evolve .-*- snapshot .-*- purge .-*- delay where

	evolve, snapshot :: IO ()
	evolve = lifecycle act ! act <<= being
	snapshot = void ! let screen = display 5
		in print <<- screen (screen <-|- run being)

--------------------------------------------------------------------------------

cube :: II Status
cube = TT . T_U ! Identity one :*: twosome (Reverse only) only where

	only :: Stream :. Zipper Stream := Status
	only = Construct one . Identity ! repeat noone

	one, noone :: Zipper Stream Status
	one = T_U ! Identity True :*: twosome # Reverse alone # alone
	noone = T_U ! Identity False :*: twosome # Reverse (repeat False) # repeat False

	alone :: Stream Status
	alone = Construct True . Identity ! repeat False

blinker :: II Status
blinker = TT . T_U ! Identity one :*: twosome # Reverse (repeat noone) # repeat noone where

	one, noone :: Zipper Stream Status
	one = T_U ! Identity True :*: twosome (Reverse alone) alone
	noone = T_U ! Identity False :*: twosome # Reverse (repeat False) # repeat False

	alone :: Stream Status
	alone = Construct True . Identity ! repeat False

main = lifecycle # conway . around # cube
