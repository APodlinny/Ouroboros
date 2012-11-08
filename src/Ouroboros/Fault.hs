module Ouroboros.Fault (
	parseFaultsFile,
	addSimilarFaults
) where

import Ouroboros.Common
import Ouroboros.Scheme
import Ouroboros.Fault.Language
import Ouroboros.Fault.Parser


addSimilarFaults :: Scheme -> Faults -> Faults
addSimilarFaults scheme fs = applySetters addFaultSetters fs
	where
		addFaultSetters = map makeFaultSetter $ faults fs
		groups = similarNames scheme

		makeFaultSetter (Node fId fType) = 
			addNodeFaults (getSameNodes groups fId) (Node fId fType)

		makeFaultSetter (Junction (from, to) fType) = 
			addJunctionFaults (
				zipWith (,) (getSameNodes groups from) (getSameNodes groups to))
			(Junction (from, to) fType)

addFault :: Fault -> Faults -> Faults
addFault f fs = Faults {
	faults = f : (faults fs)
}

addNodeFaults :: [Identifier] -> Fault -> Faults -> Faults
addNodeFaults nodeIds f fs = applySetters addFaultSetters fs
	where
		addFaultSetters = map (\x -> addNodeFault x f) nodeIds

addNodeFault :: Identifier -> Fault -> Faults -> Faults
addNodeFault nodeId f fs = addFault newFault fs
	where
		newFault = f { node = nodeId }

addJunctionFaults :: [(Identifier, Identifier)] -> Fault -> Faults -> Faults
addJunctionFaults bindings f fs = applySetters addFaultSetters fs
	where
		addFaultSetters = map (\x -> addJunctionFault x f) bindings 

addJunctionFault :: (Identifier, Identifier) -> Fault -> Faults -> Faults
addJunctionFault binding f fs = addFault newFault fs
	where
		newFault = f { nodes = binding }

getSameNodes :: [[Identifier]] -> Identifier -> [Identifier]
getSameNodes groups nodeId = filter (/= nodeId) neededGroup
	where
		neededGroups = filter (nodeId `elem`) groups
		neededGroup = concat neededGroups
