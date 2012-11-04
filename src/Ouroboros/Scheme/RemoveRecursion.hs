module Ouroboros.Scheme.RemoveRecursion (
	removeRecursions
) where

import Ouroboros.Scheme.Definition
import Ouroboros.Scheme.Common

removeRecursions :: Scheme -> Scheme
removeRecursions scheme = 
    if length recNodes == 0 then
        scheme
    else
        validateScheme "removeRecursions" $ applySetters removings scheme
    where
        recNodes = getRecursionNodes $ nodeDefinitions scheme
        removings = map removeRecursion recNodes

removeRecursion :: Identifier -> Scheme -> Scheme
removeRecursion nodeId scheme = 
    validateScheme "removeRecursion" $ applySetters setters scheme
    where
        removeDelay s = s { 
            nodeDefinitions = filter (/= (NodeDefinition nodeId DELAY)) (nodeDefinitions s) 
        }

        remapInput input s = s {
            nodeDefinitions = NodeDefinition input OUTPUT : (nodeDefinitions s),
            bindings = map (\(from, to) ->  if (from, to) == (input, nodeId) then 
                                                (from, outputId)
                                            else
                                                (from, to)) (bindings s)
        }

        remapOutputs newName outputs s = s {
            bindings = map (\(from, to) ->  if (elem to outputs) && (from == nodeId) then
                                                (newName, to)
                                            else
                                                (from, to)) (bindings s),

            stateBindings = (newName, nodeId) : (stateBindings s)
        }

        setters = [
            removeDelay,
            remapInput input,
            addInput newName,
            remapOutputs newName outputs]

        names = getNames scheme
        newName = generateNameWithPattern names nodeId
        input = fst $ head $ filter (\(from, to) -> to == nodeId) (bindings scheme)
        outputs = map snd $ filter (\(from, to) -> from == nodeId) (bindings scheme)

getRecursionNodes :: [NodeDefinition] -> [Identifier]
getRecursionNodes [] = []
getRecursionNodes (NodeDefinition nodeId DELAY : nodeDefs) = nodeId : (getRecursionNodes nodeDefs)
getRecursionNodes (_ : nodeDefs) = getRecursionNodes nodeDefs