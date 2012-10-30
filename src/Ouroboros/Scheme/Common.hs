module Ouroboros.Scheme.Common (
	addInput,
	addOutput,
	removeOutput,
	rename,
	changeType,
	getNames,
	generateName,
	generateNameWithPattern,
	applySetters,
	stateOutputs
) where

import Ouroboros.Scheme.Definition
import Data.List
import Data.List.Utils
import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Number

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

removeOutput :: Identifier -> Scheme -> Scheme
removeOutput nodeId scheme = scheme { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings scheme
        oldDefs = nodeDefinitions scheme
        newDefs = filter (/= (NodeDefinition nodeId OUTPUT)) oldDefs
        newBinds = filter (/= (nodeId, outputId)) oldBinds

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

generateName :: [Identifier] -> Identifier
generateName names = generateNameWithPattern names $ Identifier ""

generateNameWithPattern :: [Identifier] -> Identifier -> Identifier
generateNameWithPattern names pattern = 
    if elem newName names then
        generateNameWithPattern names newName
    else
        newName
    where
        newName = Identifier $ nextName $ str pattern

nextName :: String -> String
nextName str = 
	if (length parts == 1) || (not maybeIndex) || (not isInt) then
		str ++ "_[0]"
	else 
		name ++ "_[" ++ (show $ index + 1) ++ "]"
	where
		parts = split "_" str
		lastPart = last parts
		maybeIndex = (startswith "[" lastPart) && 
						(endswith "]" lastPart)

		indexStr = drop 1 $ init lastPart
		parseResult = parse (many1 $ oneOf ['0'..'9']) "" indexStr
		isInt = case parseResult of 
			Left _ -> False
			Right _ -> True

		index = read indexStr :: Int

		name = join "_" $ init parts


applySetters :: [a -> a] -> a -> a
applySetters [] x = x
applySetters (f : fs) x = applySetters fs (f x)

stateOutputs :: Scheme -> [Identifier]
stateOutputs scheme = result
	where
		defs = nodeDefinitions scheme
		outputs = filter (\d -> nodeType d == OUTPUT) defs
		primary = primaryIOs scheme
		isPrimary d = elem (nodeName d) primary
		result = map nodeName $ filter (not . isPrimary) outputs