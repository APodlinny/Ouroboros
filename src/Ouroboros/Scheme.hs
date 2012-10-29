module Ouroboros.Scheme (
   programFromScheme,
   programToScheme,
   removeRecursions
) where
		
import Ouroboros.Scheme.FromScheme
import Ouroboros.Scheme.ToScheme
import Ouroboros.Scheme.Definition
import Ouroboros.Common
import qualified Ouroboros.Language as AST
import Data.List

removeRecursions :: Scheme -> Scheme
removeRecursions scheme = 
    if length recNodes == 0 then
        scheme
    else
        applySetters removings scheme
    where
        recNodes = getRecursionNodes $ nodeDefinitions scheme
        removings = map removeRecursion recNodes

removeRecursion :: Identifier -> Scheme -> Scheme
removeRecursion nodeId scheme = applySetters setters scheme
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
                                                (from, to)) (bindings s)
        }

        setters = [
            removeDelay,
            remapInput input,
            addInput newName,
            remapOutputs newName outputs]

        newName = generateName scheme
        input = fst $ head $ filter (\(from, to) -> to == nodeId) (bindings scheme)
        outputs = map snd $ filter (\(from, to) -> from == nodeId) (bindings scheme)

getRecursionNodes :: [NodeDefinition] -> [Identifier]
getRecursionNodes [] = []
getRecursionNodes (NodeDefinition nodeId DELAY : nodeDefs) = nodeId : (getRecursionNodes nodeDefs)
getRecursionNodes (_ : nodeDefs) = getRecursionNodes nodeDefs

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

rename :: Identifier -> Identifier -> Scheme -> Scheme
rename nodeA nodeB scheme = scheme { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings scheme
        oldDefs = nodeDefinitions scheme
        newDefs = map renameDef oldDefs
        newBinds = map renameBind oldBinds
        renameBind (a, b) = if a == nodeA then
                                (nodeB, b)
                            else if b == nodeA then
                                (a, nodeB)
                            else
                                (a, b)

        renameDef def = if (nodeName def) == nodeA then
                            def { nodeName = nodeB }
                        else
                            def

changeType :: Identifier -> NodeType -> Scheme -> Scheme
changeType nodeId nodeT scheme = scheme { nodeDefinitions = newDefs }
    where
        oldDefs = nodeDefinitions scheme
        newDefs = map changeT oldDefs
        changeT def =   if (nodeName def) == nodeId then
                            if ((nodeType def) /= INPUT) && ((nodeType def) /= OUTPUT) then
                                (NodeDefinition nodeId nodeT)
                            else
                                def
                        else
                            def

getNames :: Scheme -> [Identifier]
getNames =  nub .
            map (\ (NodeDefinition name _) -> name) .
            nodeDefinitions

generateName :: Scheme -> Identifier
generateName scheme = generateNameWithPattern names pattern
    where
        names = getNames scheme
        pattern =   if length names == 0 then 
                        AST.Identifier "0"
                    else 
                        head names

generateNameWithPattern :: [Identifier] -> Identifier -> Identifier
generateNameWithPattern names pattern = 
    if elem newName names then
        generateNameWithPattern names newName
    else
        newName
    where
        newName = AST.Identifier $ "_" ++ AST.str pattern