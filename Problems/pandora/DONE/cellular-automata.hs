import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Control.Concurrent (threadDelay)
import "base" Data.List (take)

import Gears.Instances ()
import Gears.Utils (stream_to_list)

import Prelude (IO, Char, Int, putStr, print, reverse, (<>))

type Status = Boolean

type Neighbours = Status :*: Status :*: Status

start :: Zipper Stream Status
start = let desert = repeat False in
	imply @(Tape Stream _) <-- True <-- desert <-- desert

sierpinski :: Neighbours -> Status
sierpinski (True :*: True :*: True) = False
sierpinski (True :*: True :*: False) = True
sierpinski (True :*: False :*: True) = False
sierpinski (True :*: False :*: False) = True
sierpinski (False :*: True :*: True) = True
sierpinski (False :*: True :*: False) = False
sierpinski (False :*: False :*: True) = True
sierpinski (False :*: False :*: False) = False

neighbourhood :: Zipper Stream Status -> Neighbours
neighbourhood z = cell @Left z :*: cell @Root z :*: cell @Right z

cell :: forall mod i . (Extractable i, Substructured mod (Tape Stream) i) => Tape Stream Status -> Status
cell z = extract <--- view <-- sub @mod <-- z

-- TODO: refactor, but characters immediately, don't convert to lists
display :: Int -> Zipper Stream a -> [a]
display n (run . lower -> Exactly x :*: (T_U (Reverse bs :*: fs))) = reverse (take n <-- stream_to_list bs) <> [x] <> take n (stream_to_list fs)

record :: (Zipper Stream Status -> Status) -> Zipper Stream Status -> IO ()
record act being = evolve .-*- snapshot .-*- delay where

	evolve, snapshot :: IO ()
	evolve = record act <-- act <<= being
	snapshot = print <-- display 25 being

delay, purge :: IO ()
delay = threadDelay 1000000
purge = putStr "\ESC[2J"

type II = Tape Stream <::> Tape Stream
type Horizontally = Tape Stream <::> Stream
type Vertically = Stream <::> Tape Stream

instance Extendable (->) II where
	f <<= zz = f <-|--- TT <--- horizontal <-|- vertical zz where

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
conway (focused :*: neighbors) = iff @True
	<------ alive == one + one 
	<------ focused 
	<------ iff @True 
		<----- alive == one + one + one 
		<----- True 
		<----- False where

	alive :: Int
	alive = reduce count zero neighbors
	
	count :: Boolean -> Int -> Int
	count status acc = iff @True <----- status <----- acc + one <----- acc

lifecycle :: (II Status -> Status) -> II Status -> IO ()
lifecycle act being = evolve .-*- snapshot .-*- purge .-*- delay where

	evolve, snapshot :: IO ()
	evolve = lifecycle act <-- act <<= being
	snapshot = void <----- print <<---- display 5 <--- display 5 <-|- run being

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

main = lifecycle <-- conway . around <-- cube
--main = record <-- sierpinski . neighbourhood <-- start
