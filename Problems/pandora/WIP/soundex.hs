import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" System.IO (print)
import "base" Text.Show (Show)

import Gears.Instances ()

encode :: Letter -> Maybe Number
encode A = Nothing
encode B = Just N1
encode C = Just N2
encode D = Just N3
encode E = Nothing
encode F = Just N1
encode G = Just N2
encode H = Nothing
encode I = Nothing
encode J = Just N2
encode K = Just N2
encode L = Just N4
encode M = Just N5
encode N = Just N5
encode O = Nothing
encode P = Just N1
encode Q = Just N2
encode R = Just N6
encode S = Just N2
encode T = Just N3
encode U = Nothing
encode V = Just N1
encode W = Nothing
encode X = Just N2
encode Y = Nothing
encode Z = Just N2

compress :: Letter -> Maybe Number -> Maybe Number
compress letter previous = let next = encode letter in
	next == previous ? Nothing $ next

soundex :: Letter -> State (Maybe Number) (Maybe Number)
soundex letter = current @(Maybe Number) >>= \previous ->
	let result = compress letter previous in
		replace @(Maybe Number) result

robert :: Nonempty List Letter
robert = item @Push R $ item @Push O $ item @Push B $ item @Push E $ item @Push R $ point T

jackson :: Nonempty List Letter
jackson = item @Push J $ item @Push A $ item @Push C $ item @Push K $ item @Push S $ item @Push O $ point N

deriving instance Show Letter
deriving instance Show Number
deriving instance Show Control
deriving instance Show Sign
deriving instance Show Case
deriving instance Show Quote
deriving instance Show Bracket
deriving instance Show Slash
deriving instance Show Position
deriving instance Show ASCII

recompile :: Nonempty List Letter -> Nonempty List ASCII
recompile word = Construct # Letter Upper (extract word) # run (Number <$> normalize tail) where

	tail :: List (Maybe Number)
	tail = extract . run % empty $ unite @List (deconstruct word) ->> soundex

	normalize :: List (Maybe Number) -> List Number
	normalize = reduce @(Maybe Number) (\x r -> resolve @Number (item @Push % r) r x) empty

main = print $ recompile robert
