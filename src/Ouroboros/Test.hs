module Ouroboros.Test (
	Tests(..),
	TextBlock(..),
	parseTestsFile,
	repackTests,
	unpackTests,
	areTestsPacked,
	isTestPacked,
	getUntestedFaults,
	getTestedFaults,
	removeUnnecessaryInfo,
	addTextBlocks,
	removeTestsInfo,
	getTestByFault,
	isFaultBlock
) where

import Data.List
import Data.Maybe

import Ouroboros.Common
import Ouroboros.Test.Parser
import Ouroboros.Test.Language
import Ouroboros.Scheme
import Ouroboros.Scheme.Common

addTextBlocks :: Tests -> [TextBlock] -> Tests
addTextBlocks tests newBlocks = Tests {
	blocks = (blocks tests) ++ newBlocks
}

removeTestsInfo :: Tests -> Tests
removeTestsInfo tests = Tests {
	blocks = filter (not . isFaultBlock) $ blocks tests
}

getTestByFault :: Tests -> Fault -> Maybe TextBlock
getTestByFault tests neededFault = result
	where
		faultBlocks = filter (\x -> (isFaultBlock x) && (fault x == neededFault)) $ blocks tests
		result = case length faultBlocks of
			0 -> Nothing
			_ -> Just $ head faultBlocks

removeUnnecessaryInfo :: Scheme -> Tests -> Tests
removeUnnecessaryInfo scheme tests = result
	where
		primary = primaryIOs scheme
		primaryIndices = primaryInputs ++ primaryOutputs
		necessary = separatorIndex : primaryIndices
		separatorIndex = ioIndexDiff - 1

		primaryInputs = getIndices (`elem` primary) id $ getInputsList tests
		primaryOutputs = getIndices (`elem` primary) (+ ioIndexDiff) $ getOutputsList tests

		ioIndexDiff = 1 + (length $ getInputsList tests)
		result = Tests $ map removeUnnecesarryInfoFromBlock $ blocks tests

		removeUnnecesarryInfoFromBlock (Comment str) = Comment str
		removeUnnecesarryInfoFromBlock (InputsList list) = InputsList $ filter (`elem` primary) list
		removeUnnecesarryInfoFromBlock (OutputsList list) = OutputsList $ filter (`elem` primary) list
		removeUnnecesarryInfoFromBlock (FaultDescription f []) = FaultDescription f []
		removeUnnecesarryInfoFromBlock (FaultDescription f ts) = FaultDescription {
			fault = f,
			tests = map removeUnnecessary ts
		}
			where
				removeUnnecessary test = map (test !!) $ filter (`elem` necessary) $ [0 .. length test - 1]

getTestedFaults :: Scheme -> Tests -> [Fault]
getTestedFaults scheme tests = filter (isTestPacked scheme tests) faultsFromTest
	where  
		faultsFromTest = map fault $ filter isFaultBlock $ blocks tests

getUntestedFaults :: Scheme -> Tests -> [Fault]
getUntestedFaults scheme tests = filter (not . isTestPacked scheme tests) faultsFromTest
	where  
		faultsFromTest = map fault $ filter isFaultBlock $ blocks tests

areTestsPacked :: Scheme -> Tests -> Bool
areTestsPacked scheme tests = all (isTestPacked scheme tests) faultsFromTest
	where
		faultsFromTest = map fault $ filter isFaultBlock $ blocks tests

isTestPacked :: Scheme -> Tests -> Fault -> Bool
isTestPacked scheme tests faultA = result
	where
		faultBlocks = filter isFaultBlock $ blocks tests
		neededBlocks = filter (\x -> fault x == faultA) faultBlocks
		result = case neededBlocks of
			[] -> False
			(x:_) -> isBlockPacked scheme tests x

isBlockPacked :: Scheme -> Tests -> TextBlock -> Bool
isBlockPacked scheme tests (FaultDescription _ vectors) = any (all (== 'x') . statePositions) vectors
	where
		state = stateIOs scheme
		stateIndices = stateInputIndices
		stateInputIndices = getIndices (`elem` state) id $ getInputsList tests
		statePositions x = map (x !!) stateIndices

isBlockPacked _ _ _ = True

repackTests :: Scheme -> Tests -> Tests
repackTests scheme tests = Tests {
	blocks = map repackBlock $ blocks tests
}
	where
		repackBlock = repackTestsGroup varInfo
		varInfo = (nonStateIndices, stateIndices, primaryIndices)

		state = stateIOs scheme
		nonState = filter (not . (`elem` state)) $ getNames scheme
		primary = primaryIOs scheme

		stateIndices = stateInputIndices ++ stateOutputIndices
		nonStateIndices = nonStateInputIndices ++ nonStateOutputIndices
		primaryIndices = primaryInputIndices ++ primaryOutputIndices

		stateInputIndices = getIndices (`elem` state) id $ getInputsList tests
		stateOutputIndices = getIndices (`elem` state) (+ ioIndexDiff) $ getOutputsList tests

		nonStateInputIndices = getIndices (`elem` nonState) id $ getInputsList tests
		nonStateOutputIndices = getIndices (`elem` nonState) (+ ioIndexDiff) $ getOutputsList tests

		primaryInputIndices = getIndices (`elem` primary) id $ getInputsList tests
		primaryOutputIndices = getIndices (`elem` primary) (+ ioIndexDiff) $ getOutputsList tests		

		ioIndexDiff = 1 + (length $ getInputsList tests)

repackTestsGroup :: ([Int], [Int], [Int]) -> TextBlock -> TextBlock
repackTestsGroup portsInfo (FaultDescription f ts) = FaultDescription {
	fault = f,
	tests = packTests portsInfo $ unpackTests ts
}
repackTestsGroup _ x = x

packTests :: ([Int], [Int], [Int]) -> [String] -> [String]
packTests (ports, states, primary) tests = 
	uniqueTestsAt primary $
	map fromJust $ 
	filter isJust packedGroups
	
	where
		unique = nub tests
		grouped = groupby (portVars ports) unique
		portVars indices v = map (v !!) indices
		stateCombinations = 2 ^ (length states)
		packedGroups = map packGroup grouped
		indices = [0 .. (length $ head tests) - 1]
		
		packGroup :: [String] -> Maybe String
		packGroup group = if (length group < stateCombinations) || (length group == 1) then
								Nothing
						  else
								Just $ map setter indices
						  where
								setter i = if elem i states then 'x' else (head group) !! i

unpackTests :: [String] -> [String]
unpackTests [] = []
unpackTests (t : ts) = 
	case xPosition of
		Nothing -> t : (unpackTests ts)
		Just n -> unpackTests ((ta n) : (tb n) : ts)
	where
		xPosition = elemIndex 'x' t
		ta pos = setVar pos '0' t
		tb pos = setVar pos '1' t

getVar :: Int -> String -> Char
getVar position vector = head $ drop position vector

setVar :: Int -> Char -> String -> String
setVar position bit vector = map setter indices
	where
		indices = [0 .. (length vector) - 1]
		setter i = if i == position then bit else vector !! i

uniqueTestsAt :: [Int] -> [String] -> [String]
uniqueTestsAt indices vectors = nubBy comparer vectors
	where
		comparer a b = (selector a) == (selector b)
		selector x = map (x !!) indices

getInputsList :: Tests -> [Identifier]
getInputsList = getIOs isInputsBlock

getOutputsList :: Tests -> [Identifier]
getOutputsList = getIOs isOutputsBlock

getIOs :: (TextBlock -> Bool) -> Tests -> [Identifier]
getIOs criteria = 
	concat . 
	map nodeList . 
	filter criteria . 
	blocks

isInputsBlock :: TextBlock -> Bool
isInputsBlock (InputsList _) = True
isInputsBlock _ = False

isOutputsBlock :: TextBlock -> Bool
isOutputsBlock (OutputsList _) = True
isOutputsBlock _ = False

isFaultBlock :: TextBlock -> Bool
isFaultBlock (FaultDescription _ _) = True
isFaultBlock _ = False

getIndices :: (Identifier -> Bool) -> (Int -> Int) -> [Identifier] -> [Int]
getIndices criteria indexFunc source = 
	map fst $
	filter (criteria . snd) $
	mapIndex (\i x -> (indexFunc i, x)) $
	source