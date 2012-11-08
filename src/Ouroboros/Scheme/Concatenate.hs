module Ouroboros.Scheme.Concatenate (
	concatSchemes
) where

import Ouroboros.Scheme.Common
import Ouroboros.Scheme.Definition

concatSchemes :: [(Identifier, Identifier)] -> Scheme -> Scheme -> Scheme
concatSchemes nodesToBind schemeA schemeB = 
	validateScheme "concatSchemes" $ 
	applySetters concatSetters concatedSchemes
	where
		concatedSchemes = Scheme {
			name = name schemeA,
			bindings = (bindings schemeA) ++ (bindings schemeB),
			nodeDefinitions = (nodeDefinitions schemeA) ++ (nodeDefinitions schemeB),
			primaryIOs = (primaryIOs schemeB) ++ (primaryInputs schemeA),
			stateBindings = zipWith bindedSB (stateBindings schemeA) (stateBindings schemeB)
		}

		inputs s = map nodeName $ 
			filter (\def -> nodeType def == INPUT) $ 
			nodeDefinitions schemeA
		
		primaryInputs s = filter (\x -> elem x $ primaryIOs s) $ inputs s
		concatSetters = map concatSetter nodesToBind
		bindedSB (a, b) (c, d) = (a, d)

concatSetter :: (Identifier, Identifier) -> Scheme -> Scheme
concatSetter (from, to) s = applySetters concatAlgo s
	where
		concatAlgo = [
			removeInput to,
			removeOutput from,
			rename to from]