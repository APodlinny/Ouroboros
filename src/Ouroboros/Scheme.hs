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
   copySchemeTimes,
   concatSchemes
) where

import Ouroboros.Scheme.Definition
import Ouroboros.Scheme.FromScheme
import Ouroboros.Scheme.ToScheme
import Ouroboros.Scheme.RemoveRecursion
import Ouroboros.Scheme.Deafen
import Ouroboros.Scheme.Copy
import Ouroboros.Scheme.Concatenate
import Ouroboros.Scheme.Common

copySchemeTimes :: Int -> Scheme -> Scheme
copySchemeTimes 1 s = s
copySchemeTimes n s = copySchemeTimes (n - 1) concated
   where
      concated = concatSchemes binds s copyS
      copyS = copyScheme s
      binds = zipWith (,) (stateOutputs s) (reverse $ stateInputs copyS)