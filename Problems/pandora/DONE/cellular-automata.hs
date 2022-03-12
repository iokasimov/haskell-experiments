import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Control.Concurrent (threadDelay)
import "base" Data.List (take)

import Gears.Instances ()
import Gears.Utils (stream_to_list)

import Prelude (IO, Char, Int, putStr, putChar, print, signum, reverse, (<>))

type Status = Boolean

status_char :: Status -> Char
status_char False = ' '
status_char True = '*'

type Neighbours = Status :*: Status :*: Status

start :: Zipper Stream Status
start = let desert = repeat False in
	imply @(Tape Stream _) <-- True <-- desert <-- desert

sierpinski :: Neighbours -> Status
sierpinski (l :*: _ :*: r) = (l == r) != True

neighbourhood :: Zipper Stream Status -> Neighbours
neighbourhood z = cell @Left z :*: cell @Root z :*: cell @Right z

cell :: forall mod i . (Extractable i, Substructured mod (Tape Stream) i) => Tape Stream Status -> Status
cell z = extract <--- view <-- sub @mod <-- z

print_cell :: Status -> IO ()
print_cell False = putChar ' ' .-*- putChar ' '
print_cell True = putChar ' ' .-*- putChar '*'

display_field :: Int -> Zipper Stream (Zipper Stream Status) -> IO ()
display_field n zz =
	first_items_in_stream n (display_line n) (view <-- sub @Right <-- zz)
	.-*--- display_line n (extract <--- view <-- sub @Root <-- zz)
	.-*--- run (first_items_in_stream n (Backwards . display_line n) (run <--- view <-- sub @Left <-- zz))

display_line :: Int -> Zipper Stream Status -> IO ()
display_line n z = putStr "\n"
	.-*--- first_items_in_stream n print_cell (view <-- sub @Right <-- z)
	.-*--- print_cell (extract <--- view <-- sub @Root <-- z)
	.-*--- run (first_items_in_stream n (Backwards . print_cell) (run <--- view <-- sub @Left <-- z))

first_items_in_stream :: (Covariant (->) (->) t, Monoidal (-->) (-->) (:*:) (:*:) t)
	=> Int -> (a -> t ()) -> Stream a -> t ()
first_items_in_stream n f (Construct x xs) = case signum n of
	1 -> first_items_in_stream (n - 1) f (extract xs) .-*- f x
	-1 -> point ()
	0 -> point ()

record :: (Zipper Stream Status -> Status) -> Zipper Stream Status -> IO ()
record act being = evolve .-*- snapshot .-*- delay where

	evolve, snapshot :: IO ()
	evolve = record act <--- act <<= being
	snapshot = display_line 25 being

delay, purge :: IO ()
delay = threadDelay 1000000
purge = putStr "\ESC[2J"

type II = Tape Stream <::> Tape Stream
type Horizontally = Tape Stream <::> Stream
type Vertically = Stream <::> Tape Stream

instance Extendable (->) II where
	f <<= zz = f <-|--- TT <---- horizontal <-|- vertical zz where

		horizontal, vertical :: II a -> Tape Stream (II a)
		horizontal z = imply @(Tape Stream _) <-- z <-- move ((rotate @Left <-|-) =#-) z <-- move ((rotate @Right <-|-) =#-) z
		vertical z = imply @(Tape Stream _) <-- z <-- move (rotate @Left =#-) z <-- move (rotate @Right =#-) z

		move :: (Extractable t, Covariant (->) (->) t, Monoidal (-->) (-->) (:*:) (:*:) t) => (a -> a) -> a -> Construction t a
		move f x = extract . deconstruct <------- point . f .-+ x

-- TODO: try to use some datastructure tha resembles cell neighborhood
type Around = Status -- current
	:*: Status :*: Status -- horizontal
	:*: Status :*: Status -- vertical
	:*: Status :*: Status -- major diagonal
	:*: Status :*: Status -- minor diagonal

around :: II Status -> Around
around z = extract z :*: plane @(All Left) :*: plane @(All Right) :*: plane @Up :*: plane @Down
	:*: slant @Down @Left :*: slant @Up @Right :*: slant @Up @Left :*: slant @Down @Right where

	plane :: forall i t u . (Substructured i II (t <::> u), Extractable t, Covariant (->) (->) u, Extractable u) => Status
	plane = extract . lower <--- view <-- sub @i <-- z

	slant :: forall v h t u . (Substructured v II (t <::> Tape Stream), Extractable t, Substructured h (Tape Stream) u, Extractable u) => Status
	slant = extract . view (sub @h) . lower <--- view <-- sub @v <-- z

conway :: Around -> Status
conway (focused :*: neighbors) = (alive == one + one) ?= True <--- focused
	<--- (alive == one + one + one) ?= True <-- True <-- False where

	alive :: Int
	alive = reduce count zero neighbors

	count :: Boolean -> Int -> Int
	count status acc = status ?= True
		<-- acc + one
		<-- acc

lifecycle :: (II Status -> Status) -> II Status -> IO ()
lifecycle act being = evolve .-*- snapshot .-*- purge .-*- delay where

	evolve, snapshot :: IO ()
	evolve = lifecycle act <--- act <<= being
	snapshot = void <----- display_field 5 <-- run being

cube :: II Status
cube = TT ---> imply @(Tape Stream _) <-- thrice <-- only <-- only where

	only :: Stream :. Zipper Stream > Status
	only = Construct thrice . Exactly <-- repeat noone

blinker :: II Status
blinker = TT ---> imply @(Tape Stream _) <-- thrice <-- repeat noone <-- repeat noone

thrice, noone :: Zipper Stream Status
thrice = imply @(Tape Stream _) <-- True <-- alone <-- alone
noone = imply @(Tape Stream _) <-- False <-- repeat False <-- repeat False

alone :: Stream Status
alone = Construct True . Exactly <-- repeat False

-- main = record <-- sierpinski . neighbourhood <-- start
main = lifecycle <-- conway . around <-- cube
