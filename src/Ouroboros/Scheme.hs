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

import Data.List hiding (concat)
import Prelude hiding (concat)

import Ouroboros.Scheme.Definition
import Ouroboros.Scheme.FromScheme
import Ouroboros.Scheme.ToScheme
import Ouroboros.Scheme.RemoveRecursion
import Ouroboros.Scheme.Deafen
import Ouroboros.Scheme.Copy
import Ouroboros.Scheme.Concatenate
import Ouroboros.Scheme.Common

copySchemeTimes :: Int -> Scheme -> Scheme
copySchemeTimes count s = iterate count s s
   where
      iterate 1 lastScheme  _ = lastScheme

      iterate count lastScheme source = iterate (count - 1) concated newScheme
         where
            concated = concat lastScheme newScheme
            newScheme = copyScheme source

concat :: Scheme -> Scheme -> Scheme
concat s1 s2 = concated
   where
      stateBinds1 = stateBindings s1
      stateBinds2 = stateBindings s2
      concated = concatSchemes binds s1 s2

      binds = zipWith rebind stateBinds1 stateBinds2
      rebind (a, b) (c, d) = (b, c)