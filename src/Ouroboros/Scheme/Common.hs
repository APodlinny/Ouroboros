module Ouroboros.Scheme.Common (
	addInput,
	addOutput,
	removeOutput,
    removeInput,
	rename,
	changeType,
	getNames,
	generateName,
	generateNameWithPattern,
	applySetters,
    validateScheme,
    nonPrimaryOutputs,
    nonPrimaryInputs,
    inputPorts,
    outputPorts,
    ioPorts,
    stateIOs,
    nonStateIOs,
    getNameWithoutIndex,
    similarNames,
    logger
) where

import Data.List
import qualified Data.Set as Set 

import Ouroboros.Common
import Ouroboros.Scheme.Definition

validateScheme :: String -> Scheme -> Scheme
validateScheme context s = 
    if validateBindings context $ bindings s then
        s
    else
        error $ context ++ ": scheme is not valid."

validateBindings :: String -> [Binding] -> Bool
validateBindings context bindings = result
    where
        result = iter bindings Set.empty Set.empty Set.empty

        iter :: [Binding] -> Set.Set Identifier -> Set.Set Identifier -> Set.Set Identifier -> Bool
        iter [] vars inVars outVars = all id $ Set.elems $ Set.map bothInAndOut vars
            where
                    bothInAndOut v =    if (v == inputId) || (v == outputId) then
                                            True
                                        else if not (Set.member v inVars) then
                                            error $ context ++ ": Unspecified node: " ++ show v 
                                        else if not (Set.member v outVars) then
                                            error $ context ++ ": Unknown node: " ++ show v
                                        else
                                            True

        iter (b : bs) vars inVars outVars = iter bs (Set.insert (fst b) (Set.insert (snd b) vars))
                                                    (Set.insert (fst b) inVars)
                                                    (Set.insert (snd b) outVars)

addInput :: Identifier -> Scheme -> Scheme
addInput nodeId s = s { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings s
        oldDefs = nodeDefinitions s
        newDefs = NodeDefinition nodeId INPUT : oldDefs
        newBinds = (inputId, nodeId) : oldBinds
        
addOutput :: Identifier -> Scheme -> Scheme
addOutput nodeId s = s { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings s
        oldDefs = nodeDefinitions s
        newDefs = NodeDefinition nodeId OUTPUT : oldDefs
        newBinds = (nodeId, outputId) : oldBinds

removeOutput :: Identifier -> Scheme -> Scheme
removeOutput nodeId s = s { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings s
        oldDefs = nodeDefinitions s
        newDefs = filter (/= (NodeDefinition nodeId OUTPUT)) oldDefs
        newBinds = filter (/= (nodeId, outputId)) oldBinds

removeInput :: Identifier -> Scheme -> Scheme
removeInput nodeId s = s { bindings = newBinds, nodeDefinitions = newDefs }
    where
        oldBinds = bindings s
        oldDefs = nodeDefinitions s
        newDefs = filter (/= (NodeDefinition nodeId INPUT)) oldDefs
        newBinds = filter (/= (inputId, nodeId)) oldBinds  

rename :: Identifier -> Identifier -> Scheme -> Scheme
rename nodeA nodeB s = s { 
        bindings = newBinds, 
        nodeDefinitions = newDefs,
        primaryIOs = newPrimaryIOs,
        stateBindings = newStateBindings
    }
    where
        oldBinds = bindings s
        oldDefs = nodeDefinitions s
        oldPrimaryIOs = primaryIOs s
        oldStateBindings = stateBindings s

        newDefs = map renameDef oldDefs
        newBinds = map renameBind oldBinds
        newPrimaryIOs = map renameIO oldPrimaryIOs
        newStateBindings = map renameBind oldStateBindings

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

        renameIO x =    if x == nodeA then
                            nodeB
                        else
                            x

changeType :: Identifier -> NodeType -> Scheme -> Scheme
changeType nodeId nodeT s = s { nodeDefinitions = newDefs }
    where
        oldDefs = nodeDefinitions s
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
		parts = split '_' str
		lastPart = last parts
		maybeIndex = (startswith "[" lastPart) && 
						(endswith "]" lastPart)

		indexStr = drop 1 $ init lastPart
		isInt = case catchBottom index of 
			Nothing -> False
			Just _  -> True

		index = read indexStr :: Int

		name = join "_" $ init parts

similarNames :: Scheme -> [[Identifier]]
similarNames scheme = 
    filter (\x -> length x /= 1) $
    groupby (getNameWithoutIndex . str) $
    getNames scheme

nameMatchesPattern :: String -> String -> Bool
nameMatchesPattern pattern str = 
    (getNameWithoutIndex pattern) == (getNameWithoutIndex str)

getNameWithoutIndex :: String -> String
getNameWithoutIndex str = getName str
    where
        parts x = split '_' x
        name x = join "_" $ init $ parts x
        lastPart x = last $ parts x
        hasIndex x = (startswith "[" $ lastPart x) &&
                    (endswith "]" $ lastPart x) &&
                    (isInt $ drop 1 $ init $ lastPart x)

        isInt x = case catchBottom (read x :: Int) of
            Nothing -> False
            Just _ -> True

        getName x = 
            if hasIndex x then
                name x
            else
                x

nonPrimaryInputs :: Scheme -> [Identifier]
nonPrimaryInputs = nonPrimaryPorts INPUT

nonPrimaryOutputs :: Scheme -> [Identifier]
nonPrimaryOutputs = nonPrimaryPorts OUTPUT

nonPrimaryPorts :: NodeType -> Scheme -> [Identifier]
nonPrimaryPorts node scheme = result
    where
        defs = nodeDefinitions scheme
        ports = filter (\d -> nodeType d == node) defs
        primary = primaryIOs scheme
        isPrimary d = elem (nodeName d) primary
        result = map nodeName $ filter (not . isPrimary) ports

stateIOs :: Scheme -> [Identifier]
stateIOs scheme = 
    nub $ 
    concat $ 
    map (\(a, b) -> [a, b]) $
    stateBindings scheme

nonStateIOs :: Scheme -> [Identifier]
nonStateIOs scheme = 
    filter (\x -> not $ 
                elem x $ 
                stateIOs scheme) $ 
    map nodeName $ 
    ioPorts scheme

inputPorts :: Scheme -> [Identifier]
inputPorts = map nodeName . filter (\d -> nodeType d == INPUT) . ioPorts

outputPorts :: Scheme -> [Identifier]
outputPorts = map nodeName . filter (\d -> nodeType d == OUTPUT) . ioPorts

ioPorts :: Scheme -> [NodeDefinition]
ioPorts scheme = ports
    where
        defs = nodeDefinitions scheme
        ports = filter isIO defs
        isIO x = (nodeType x == INPUT) || (nodeType x == OUTPUT)