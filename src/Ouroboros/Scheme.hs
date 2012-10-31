module Ouroboros.Scheme (
   programFromScheme,
   programToScheme,
   removeRecursions,
   deafenStateOutputs,
   Scheme(..),
   Binding,
   NodeDefinition(..),
   NodeType(..),
   Identifier(..),
   inputId,
   outputId,
   copyScheme,
   copySchemeN,
   concatSchemes
) where
		
import Ouroboros.Scheme.Definition
import Ouroboros.Scheme.FromScheme
import Ouroboros.Scheme.ToScheme
import Ouroboros.Scheme.RemoveRecursion
import Ouroboros.Scheme.Deafen
import Ouroboros.Scheme.Copy
import Ouroboros.Scheme.Concatenate

copySchemeN :: Int -> Scheme -> Scheme
copySchemeN 1 s = s
copySchemeN n s = copySchemeN (n - 1) concated
   where
      concated = undefined