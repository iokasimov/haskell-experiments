import "base" Data.Char (Char)
import "base" System.IO (print)
import "base" Text.Show (Show (show))

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import Gears.Instances

data Variable = A | B

deriving instance Show Variable

algae :: Variable -> Comprehension Maybe Variable
algae A = Comprehension $ A += B += empty
algae B = Comprehension $ A += empty

--------------------------------------------------------------------------------

start :: Comprehension Maybe Variable
start = Comprehension $ A += empty

main = print . run $ start >>= algae >>= algae >>= algae >>= algae
