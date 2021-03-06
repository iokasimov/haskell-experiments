import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" System.IO (print)

import Gears.Instances

-- I need a queue to implement such an instance
-- instance Morphable (Into (Levelorder (Construction Maybe))) (Construction Wye) where
	-- type Morphing (Into (Levelorder (Construction Maybe))) (Construction Wye) = Construction Maybe

example = Construct 1 $ Both
	(Construct 2 $ Both (Construct 4 End) (Construct 5 End))
	(Construct 3 . Right $ Construct 6 End)

main = void $ do
	print "--------------------- Preorder -----------------------"
	print $ into @(Preorder (Nonempty List)) example
	print "--------------------- Postorder -----------------------"
	print $ into @(Postorder (Nonempty List)) example
	print "--------------------- Inorder -----------------------"
	print $ into @(Inorder (Nonempty List)) example
