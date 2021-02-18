import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" System.IO (print)

instance Morphable (Into (Levelorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Levelorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing (extract . run -> Construct x End) = point x
	-- morphing (extract . run -> Construct x (Left lst)) = point x + into @(Levelorder (Nonempty Stack)) lst
	-- morphing (extract . run -> Construct x (Right rst)) = point x + into @(Levelorder (Nonempty Stack)) lst
	-- morphing (extract . run -> Construct x (Both lst rst)) = point x + extract lst + extract rst +

example = Construct 1 $ Both (Construct 2 $ Both (Construct 4 End) (Construct 5 End)) (Construct 3 . Right $ Construct 6 End)

main = void $ do
	into @(Preorder (Nonempty Stack)) example ->> print
	into @(Postorder (Nonempty Stack)) example ->> print
	into @(Inorder (Nonempty Stack)) example ->> print
