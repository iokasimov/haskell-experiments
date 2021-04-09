module Main where

-- Source: https://gist.github.com/pchiusano/e51c4aec00ceb9e83393

import qualified Data.Foldable as Foldable
import Data.Foldable (Foldable)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (abs)

type V = String

data ABT f a = Var V | Abs V a | Tm (f a) deriving Functor

--instance Functor f => Functor (ABT f) where
--  fmap f abt = case abt of
--    Var x -> Var x
--    Abs v a -> Abs v (f a)
--    Tm t -> Tm (fmap f t)

data Term f = Term { freevars :: Set V , out :: ABT f (Term f) }

var :: V -> Term f
var v = Term (Set.singleton v) (Var v)

abs :: V -> Term f -> Term f
abs v body = Term (Set.delete v (freevars body)) (Abs v body)

tm :: (Foldable f, Functor f) => f (Term f) -> Term f
tm t = Term (Set.unions (Foldable.toList (fmap freevars t)))
            (Tm t)

into :: (Foldable f, Functor f) => ABT f (Term f) -> Term f
into abt = case abt of
	Var x -> var x
	Abs v a -> abs v a
	Tm t -> tm t

fresh :: (V -> Bool) -> V -> V
fresh used v | used v = fresh used ("'" ++ v)
fresh _  v = v
fresh' :: Set V -> V -> V
fresh' vs v = fresh (\v -> Set.member v vs) v
rename :: (Foldable f, Functor f) => V -> V -> Term f -> Term f
rename old new (Term fvs t) = case t of
	Var v -> if v == old then var new else var old
	Abs v body -> if v == old then abs v body else abs v (rename old new body)
	Tm v -> tm (fmap (rename old new) v)

-- | `subst t x body` substitutes `t` for `x` in `body`, avoiding capture
subst :: (Foldable f, Functor f) => Term f -> V -> Term f -> Term f
subst t x body = case out body of
	Var v | x == v -> t
	Var v -> var v
	Abs x' e | x == x' -> abs x' e
	Abs x e -> abs x' e' where
		memberOf s1 s2 v = Set.member v s1 || Set.member v s2
		x' = fresh (memberOf (freevars t) (freevars body)) x
		-- rename x to something that cannot be captured
		e' = if x /= x' then subst t x (rename x x' e) else subst t x e
	Tm body -> tm (fmap (subst t x) body)

main = print "typechecked"
