module Ouroboros.Test (
	parseTestsFile,
	repackTests,
	areTestsPacked,
	isTestPacked
) where

import Data.List

import Ouroboros.Common
import Ouroboros.Test.Parser
import Ouroboros.Test.Language
import Ouroboros.Scheme
import Ouroboros.Scheme.Common

areTestsPacked :: Scheme -> Tests -> Bool
areTestsPacked scheme tests = all (isBlockPacked scheme tests) $ blocks tests

isTestPacked :: Scheme -> Tests -> Fault -> Bool
isTestPacked scheme tests faultA = result
	where
		faultBlocks = filter isFaultBlock $ blocks tests
		neededBlocks = filter (\x -> fault x == faultA) faultBlocks
		result = case neededBlocks of
			[] -> True
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
		varInfo = (nonStateIndices, stateIndices)

		state = stateIOs scheme
		nonState = nonStateIOs scheme

		stateIndices = stateInputIndices ++ stateOutputIndices
		nonStateIndices = nonStateInputIndices ++ nonStateOutputIndices

		stateInputIndices = getIndices (`elem` state) id $ getInputsList tests
		stateOutputIndices = getIndices (`elem` state) (+ ioIndexDiff) $ getOutputsList tests

		nonStateInputIndices = getIndices (`elem` nonState) id $ getInputsList tests
		nonStateOutputIndices = getIndices (`elem` nonState) (+ ioIndexDiff) $ getOutputsList tests

		ioIndexDiff = 1 + (length $ getInputsList tests)

repackTestsGroup :: ([Int], [Int]) -> TextBlock -> TextBlock
repackTestsGroup portsInfo (FaultDescription f ts) = FaultDescription {
	fault = f,
	tests = packTests portsInfo $ unpackTests ts
}
repackTestsGroup _ x = x

packTests :: ([Int], [Int]) -> [String] -> [String]
packTests (ports, states) tests = concat packedGroups
	where
		unique = uniqueTests tests
		grouped = groupby (portVars ports) unique
		portVars indices v = map (v !!) indices
		stateCombinations = 2 ^ (length states `div` 2)
		packedGroups = map packGroup grouped
		indices = [0 .. (length $ head tests) - 1]
		packGroup group = if (length group < stateCombinations) || (length group == 1) then
								group
						  else
								[map setter indices]
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

uniqueTests :: [String] -> [String]
uniqueTests = nub

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