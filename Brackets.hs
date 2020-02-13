{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Function ((&))
import Control.Joint.Abilities (lift, run, type (:>))
import Control.Joint.Effects (Stateful, Failable, State, current, modify, failure)

data Shape = Opened | Closed deriving (Eq, Show)

data Style = Round | Square | Angle | Curly deriving (Eq, Show)

data Symbol = Nevermind | Bracket Style Shape deriving Show

type Index = Integer

data Stumble
	-- Closed bracket without opened one
	= Deadend Style Index
	-- Opened bracket without closed one
	| Logjam Style Index
	-- Closed bracket doesn't match opened one
	| Mismatch Style Index Style Index
	deriving Show

type Openings = [(Style, Index)]

indexate :: Stateful Index t => t ()
indexate = modify (+ 1)

type Checker = State Index :> State Openings :> Either Stumble

after :: Checker ()
after = current @Openings >>= \case
	s : _ -> failure $ uncurry Logjam s
	[]  -> pure ()

match :: Symbol -> Checker ()
match Nevermind = pure ()
match (Bracket opened Opened) = current @Index >>= modify @Openings . (:) . (,) opened
match (Bracket closed Closed) = current @Openings >>= \case
	[] -> current @Index >>= failure . Deadend closed
	((opened, oix) : ss) -> if closed == opened then modify (const ss)
		else current @Index >>= failure . Mismatch opened oix closed

check :: Traversable s => s Symbol -> IO ()
check code = traverse (\s -> indexate *> match s) code *> after
	& flip run 1 & flip run [] & either print (const $ print "OK")

example_ok, example_mismatch, example_deadend, example_logjam :: [Symbol]
example_ok = Bracket Curly Opened : Nevermind : Bracket Curly Closed : [] -- {x}
example_mismatch = Bracket Curly Opened : Bracket Square Closed : [] -- {]
example_deadend = Bracket Round Closed : [] -- )
example_logjam = Bracket Angle Opened : [] -- <

main = check example_mismatch
