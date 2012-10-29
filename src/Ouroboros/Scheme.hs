module Ouroboros.Scheme (
   programFromScheme,
   programToScheme,
   removeRecursion
) where
		
import Ouroboros.Scheme.FromScheme
import Ouroboros.Scheme.ToScheme
import Ouroboros.Scheme.Definition

removeRecursion :: Scheme -> Scheme
removeRecursion scheme = 
    if not $ containsRecursion $ nodeDefinitions scheme then
        scheme
    else
        result
    where
        result = undefined

containsRecursion :: [NodeDefinition] -> Bool
containsRecursion [] = False
containsRecursion (NodeDefinition _ DELAY : _) = True
containsRecursion (_ : nodeDefs) = containsRecursion nodeDefs

addInput :: Identifier -> Scheme -> Scheme
addInput nodeId scheme = scheme { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings scheme
        oldDefs = nodeDefinitions scheme
        newDefs = NodeDefinition nodeId INPUT : oldDefs
        newBinds = (inputId, nodeId) : oldBinds
        
addOutput :: Identifier -> Scheme -> Scheme
addOutput nodeId scheme = scheme { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings scheme
        oldDefs = nodeDefinitions scheme
        newDefs = NodeDefinition nodeId OUTPUT : oldDefs
        newBinds = (nodeId, outputId) : oldBinds