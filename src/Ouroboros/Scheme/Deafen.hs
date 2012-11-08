module Ouroboros.Scheme.Deafen (
	deafenStateOutputs
) where

import Ouroboros.Scheme.Common
import Ouroboros.Scheme.Definition

deafenStateOutputs :: Scheme -> Scheme
deafenStateOutputs scheme = 
	validateScheme "deafenStateOutputs" $ applySetters setters scheme
	where
		setters = map deafenOutput $ nonPrimaryOutputs scheme

deafenOutput :: Identifier -> Scheme -> Scheme
deafenOutput nodeId scheme = 
	validateScheme "deafenOutput" $ applySetters setters scheme
	where
		addNot nodeName argName s = s {
			bindings = (argName, nodeName) : (bindings s),
			nodeDefinitions = NodeDefinition nodeName NOT : (nodeDefinitions s)
		}

		addAnd nodeName argNameA argNameB s = s {
			bindings = (argNameA, nodeName) : (argNameB, nodeName) : (bindings s),
			nodeDefinitions = NodeDefinition nodeName AND : (nodeDefinitions s)
		}

		setters = [
			removeOutput nodeId,
			addNot nameForNot nodeId,
			addAnd nameForAnd nodeId nameForNot,
			addOutput nameForAnd]

		names = getNames scheme
		nameForNot = generateNameWithPattern names (Identifier $ str nodeId ++ "_not")
		nameForAnd = generateNameWithPattern (nameForNot : names) (Identifier $ str nameForNot ++ "_and")