module Ouroboros.Scheme.Copy (
	copyScheme
) where

import Ouroboros.Scheme.Common
import Ouroboros.Scheme.Definition

copyScheme :: Scheme -> Scheme
copyScheme s = 
	validateScheme "copyScheme" $ applySetters renameSetters s
	where
		oldNames = getNames s
		newNames = mapGenName oldNames
		renameSetters = zipWith rename oldNames newNames

mapGenName' :: [Identifier] -> [Identifier] -> [Identifier]
mapGenName' nameBase [] = []
mapGenName' nameBase (oldName : names) = newName : (mapGenName' (newName : nameBase) names)
	where
		newName = generateNameWithPattern nameBase oldName

mapGenName :: [Identifier] -> [Identifier]
mapGenName names = mapGenName' names names