module Ouroboros.Scheme.Deafen (
	deafenStateOutputs
) where

import Ouroboros.Scheme.Common
import Ouroboros.Scheme.Definition

deafenStateOutputs :: Scheme -> Scheme
deafenStateOutputs scheme = applySetters setters scheme
	where
		setters = map deafenOutput $ stateOutputs scheme

deafenOutput :: Identifier -> Scheme -> Scheme
deafenOutput nodeId scheme = applySetters setters scheme
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
		nameForNot = generateNameWithPattern names nodeId
		nameForAnd = generateNameWithPattern (nameForNot : names) nameForNot