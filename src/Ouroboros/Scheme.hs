module Ouroboros.Scheme (
   programFromScheme,
   programToScheme,
   removeRecursions,
   deafenStateOutputs
) where
		
import Ouroboros.Scheme.FromScheme
import Ouroboros.Scheme.ToScheme
import Ouroboros.Scheme.RemoveRecursion
import Ouroboros.Scheme.Deafen