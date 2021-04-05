module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" System.IO (print)
import "base" Text.Show (Show)

data Hand = I_ | II_ | III_ | IV_ | V_ | VI_ | VII_ | VIII_ | IX_ | X_ | XI_ | XII_

data Section = S0 | S1 | S2 | S3 | S4

instance Setoid Hand where
	I_ == I_ = True
	II_ == II_ = True
	III_ == III_ = True
	IV_ == IV_ = True
	V_ == V_ = True
	VI_ == VI_ = True
	VII_ == VII_ = True
	VIII_ == VIII_ = True
	IX_ == IX_ = True
	X_ == X_ = True
	XI_ == XI_ = True
	XII_ == XII_ = True
	_ == _ = False

instance Chain Hand where
	I_ <=> I_ = Equal
	I_ <=> II_ = Less
	I_ <=> III_ = Less
	I_ <=> IV_ = Less
	I_ <=> V_ = Less
	I_ <=> VI_ = Less
	I_ <=> VII_ = Less
	I_ <=> VIII_ = Less
	I_ <=> IX_ = Less
	I_ <=> X_ = Less
	I_ <=> XI_ = Less
	I_ <=> XII_ = Less
	II_ <=> I_ = Greater
	II_ <=> II_ = Equal
	II_ <=> III_ = Less
	II_ <=> IV_ = Less
	II_ <=> V_ = Less
	II_ <=> VI_ = Less
	II_ <=> VII_ = Less
	II_ <=> VIII_ = Less
	II_ <=> IX_ = Less
	II_ <=> X_ = Less
	II_ <=> XI_ = Less
	II_ <=> XII_ = Less
	III_ <=> I_ = Greater
	III_ <=> II_ = Greater
	III_ <=> III_ = Equal
	III_ <=> IV_ = Less
	III_ <=> V_ = Less
	III_ <=> VI_ = Less
	III_ <=> VII_ = Less
	III_ <=> VIII_ = Less
	III_ <=> IX_ = Less
	III_ <=> X_ = Less
	III_ <=> XI_ = Less
	III_ <=> XII_ = Less
	IV_ <=> I_ = Greater
	IV_ <=> II_ = Greater
	IV_ <=> III_ = Greater
	IV_ <=> IV_ = Equal
	IV_ <=> V_ = Less
	IV_ <=> VI_ = Less
	IV_ <=> VII_ = Less
	IV_ <=> VIII_ = Less
	IV_ <=> IX_ = Less
	IV_ <=> X_ = Less
	IV_ <=> XI_ = Less
	IV_ <=> XII_ = Less
	V_ <=> I_ = Greater
	V_ <=> II_ = Greater
	V_ <=> III_ = Greater
	V_ <=> IV_ = Greater
	V_ <=> V_ = Equal
	V_ <=> VI_ = Less
	V_ <=> VII_ = Less
	V_ <=> VIII_ = Less
	V_ <=> IX_ = Less
	V_ <=> X_ = Less
	V_ <=> XI_ = Less
	V_ <=> XII_ = Less
	VI_ <=> I_ = Greater
	VI_ <=> II_ = Greater
	VI_ <=> III_ = Greater
	VI_ <=> IV_ = Greater
	VI_ <=> V_ = Greater
	VI_ <=> VI_ = Equal
	VI_ <=> VII_ = Less
	VI_ <=> VIII_ = Less
	VI_ <=> IX_ = Less
	VI_ <=> X_ = Less
	VI_ <=> XI_ = Less
	VI_ <=> XII_ = Less
	VII_ <=> I_ = Greater
	VII_ <=> II_ = Greater
	VII_ <=> III_ = Greater
	VII_ <=> IV_ = Greater
	VII_ <=> V_ = Greater
	VII_ <=> VI_ = Greater
	VII_ <=> VII_ = Equal
	VII_ <=> VIII_ = Less
	VII_ <=> IX_ = Less
	VII_ <=> X_ = Less
	VII_ <=> XI_ = Less
	VII_ <=> XII_ = Less
	VIII_ <=> I_ = Greater
	VIII_ <=> II_ = Greater
	VIII_ <=> III_ = Greater
	VIII_ <=> IV_ = Greater
	VIII_ <=> V_ = Greater
	VIII_ <=> VI_ = Greater
	VIII_ <=> VII_ = Greater
	VIII_ <=> VIII_ = Equal
	VIII_ <=> IX_ = Less
	VIII_ <=> X_ = Less
	VIII_ <=> XI_ = Less
	VIII_ <=> XII_ = Less
	IX_ <=> I_ = Greater
	IX_ <=> II_ = Greater
	IX_ <=> III_ = Greater
	IX_ <=> IV_ = Greater
	IX_ <=> V_ = Greater
	IX_ <=> VI_ = Greater
	IX_ <=> VII_ = Greater
	IX_ <=> VIII_ = Greater
	IX_ <=> IX_ = Equal
	IX_ <=> X_ = Less
	IX_ <=> XI_ = Less
	IX_ <=> XII_ = Less
	X_ <=> I_ = Greater
	X_ <=> II_ = Greater
	X_ <=> III_ = Greater
	X_ <=> IV_ = Greater
	X_ <=> V_ = Greater
	X_ <=> VI_ = Greater
	X_ <=> VII_ = Greater
	X_ <=> VIII_ = Greater
	X_ <=> IX_ = Greater
	X_ <=> X_ = Equal
	X_ <=> XI_ = Less
	X_ <=> XII_ = Less
	XI_ <=> I_ = Greater
	XI_ <=> II_ = Greater
	XI_ <=> III_ = Greater
	XI_ <=> IV_ = Greater
	XI_ <=> V_ = Greater
	XI_ <=> VI_ = Greater
	XI_ <=> VII_ = Greater
	XI_ <=> VIII_ = Greater
	XI_ <=> IX_ = Greater
	XI_ <=> X_ = Greater
	XI_ <=> XI_ = Equal
	XI_ <=> XII_ = Less
	XII_ <=> I_ = Greater
	XII_ <=> II_ = Greater
	XII_ <=> III_ = Greater
	XII_ <=> IV_ = Greater
	XII_ <=> V_ = Greater
	XII_ <=> VI_ = Greater
	XII_ <=> VII_ = Greater
	XII_ <=> VIII_ = Greater
	XII_ <=> IX_ = Greater
	XII_ <=> X_ = Greater
	XII_ <=> XI_ = Greater
	XII_ <=> XII_ = Equal

instance Cycle Hand where
	previous I_ = XII_
	previous II_ = I_
	previous III_ = II_
	previous IV_ = III_
	previous V_ = IV_
	previous VI_ = V_
	previous VII_ = VI_
	previous VIII_ = VII_
	previous IX_ = VIII_
	previous X_ = IX_
	previous XI_ = X_
	previous XII_ = XI_
	next I_ = II_
	next II_ = III_
	next III_ = IV_
	next IV_ = V_
	next V_ = VI_
	next VI_ = VII_
	next VII_ = VIII_
	next VIII_ = IX_
	next IX_ = X_
	next X_ = XI_
	next XI_ = XII_
	next XII_ = I_

instance Semigroup Hand where
	x + I_ = next x
	x + II_ = next $ next x
	x + III_ = next $ next $ next x
	x + IV_ = next $ next $ next $ next x
	x + V_ = next $ next $ next $ next $ next x
	x + VI_ = next $ next $ next $ next $ next $ next x
	x + VII_ = next $ next $ next $ next $ next $ next $ next x
	x + VIII_ = previous $ previous $ previous $ previous x
	x + IX_ = previous $ previous $ previous x
	x + X_ = previous $ previous x
	x + XI_ = previous x
	x + XII_ = x

instance Monoid Hand where
	zero = XII_

instance Group Hand where
	invert I_ = XI_
	invert II_ = X_
	invert III_ = IX_
	invert IV_ = VIII_
	invert V_ = VII_
	invert VI_ = VI_
	invert VII_ = V_
	invert VIII_ = IV_
	invert IX_ = III_
	invert X_ = II_
	invert XI_ = I_
	invert XII_ = XII_

deriving instance Show Hand

type Hour = Hand

type Minute = Hand :*: Section

type Face = Hour :*: Minute



main = print "typechecked"
