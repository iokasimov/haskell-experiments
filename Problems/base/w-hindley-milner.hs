module Main where

import "containers" Data.Map
import "containers" Data.Set
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import "pretty" Text.PrettyPrint

data Exp
	= EVar String
	| ELit Lit
	| EApp Exp Exp
	| EAbs String Exp
	| ELet String Exp Exp
	deriving (Eq,Ord)

data Lit
	= LInt Integer
	| LBool Bool
	deriving (Eq,Ord)

data Type
	= TVar String
	| TInt
	| TBool
	| TFun Type Type
	deriving (Eq,Ord)

data Scheme = Scheme [String] Type

-- Finite mappings from type variables to types
type Substitution = Map String Type

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Data.Map.union (apply s1 <$> s2)  s1

class Types a where
	ftv :: a -> Set String
	apply :: Substitution -> a -> a

instance Types Type where
	ftv (TVar n) = Data.Set.singleton n
	ftv TInt = mempty
	ftv TBool = mempty
	ftv (TFun st tt) = Data.Set.union (ftv st) (ftv tt)
	apply s (TFun st tt) = TFun (apply s st) (apply s tt)
	apply _ t = t

instance Types Scheme where
	ftv (Scheme vars t) = Data.Set.difference (ftv t) $ Data.Set.fromList vars
	apply s (Scheme vars t) = Scheme vars $ apply (Prelude.foldr Data.Map.delete s vars) t

instance Types a => Types [a] where
	ftv l = Prelude.foldr Data.Set.union mempty $ ftv <$> l

newtype Г = Г (Map String Scheme)

remove :: Г -> String -> Г
remove (Г env) var = Г $ Data.Map.delete var env

instance Types Г where
	ftv (Г env) = ftv $ Data.Map.elems env
	apply s (Г env) = Г $ apply s <$> env

generalize :: Г -> Type -> Scheme
generalize env t = Scheme vars t where

	vars :: [String]
	vars = Data.Set.toList $ difference (ftv t) (ftv env)

main = print "typechecked"
