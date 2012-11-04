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
    stateInputs,
	stateOutputs,
    validateScheme
) where

import Ouroboros.Scheme.Definition
import Data.List
import qualified Data.Set as Set 
import Control.Exception
import System.IO.Unsafe

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


catchBottom :: a -> Maybe a
catchBottom x = unsafePerformIO $ handle stub $ evalSafe x
    where
        stub :: SomeException -> IO (Maybe a)
        stub _ = return Nothing

        evalSafe :: a -> IO (Maybe a)
        evalSafe x = evaluate x >>= (return . Just)

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

stateInputs :: Scheme -> [Identifier]
stateInputs scheme = result
    where
        defs = nodeDefinitions scheme
        inputs = filter (\d -> nodeType d == INPUT) defs
        primary = primaryIOs scheme
        isPrimary d = elem (nodeName d) primary
        result = map nodeName $ filter (not . isPrimary) inputs
        
join :: [a] -> [[a]] -> [a]
join separator items = foldl1 joiner items
    where
        joiner a b = a ++ separator ++ b
        
split :: Eq a => a -> [a] -> [[a]]
split separator xs = reverse $ iterate spanResult []
    where
        spanResult = spanSplit separator xs
        iterate (elt, []) result = elt : result
        iterate (elt, other) result = iterate (spanSplit separator other) (elt : result)

spanSplit :: Eq a => a -> [a] -> ([a], [a])
spanSplit separator xs = (element, other)
    where
        result = span (/= separator) xs
        element = fst result
        other = drop 1 $ snd result
        
startswith :: Eq a => [a] -> [a] -> Bool
startswith begining xs = begining == (take (length begining) xs)

endswith :: Eq a => [a] -> [a] -> Bool
endswith ending xs = ending == drop (length xs - length ending) xs