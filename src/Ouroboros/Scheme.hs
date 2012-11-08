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
   concatSchemes,
   getNames,
   similarNames
) where

import Data.List

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
      s1 = s
      s2 = copyScheme s1
      stateBinds1 = stateBindings s1
      stateBinds2 = stateBindings s2
      concated = concatSchemes binds s1 s2

      binds = zipWith rebind stateBinds1 stateBinds2
      rebind (a, b) (c, d) = (b, c)