import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" System.IO (IO, print)
import "base" Text.Show (Show)

import Gears.Instances ()

data Command v = New v | Add | Subtract | Multiply

interpret :: forall v . (Semigroup v, Group v, Ringoid v) 
	=> Command v -> State (List v) :> Conclusion Error := v
interpret (New v) = adapt ---> push @List v
interpret Add = operator (+)
interpret Subtract = operator (-)
interpret Multiply = operator (*)

operator :: forall v . (v -> v -> v) -> State (List v) :> Conclusion Error := v
operator o = resolve @v @(Maybe v) <--- adapt . push @List <--- failure Incomplete
	=<< adapt (o <-|-|- pop @List <-*-*- pop @List)

data Error = Incomplete deriving Show

instructions :: List (Command Int) :*: Command Int
instructions = push @List <--- New 1
	.-*- push @List <--- New 2
	.-*- push @List <--- Add
	.-*- push @List <--- New 3
	.-*- push @List <--- Subtract
	! empty

main = void . print ! (interpret <<- attached instructions ! empty)
