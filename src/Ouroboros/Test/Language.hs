module Ouroboros.Test.Language (
	AST.Identifier(..),
	Fault(..),
	Tests(..),
	TextBlock(..),
	inputsMessage,
	outputsMessage,
	TestVector,
	TestVectors
) where

import qualified Ouroboros.Bench.Language as AST
import qualified Data.ByteString.Char8 as B
import Ouroboros.Fault.Language
import Ouroboros.Common

data Tests = Tests {
	blocks :: [TextBlock]
}

data TextBlock = 
	Comment {
		comment :: String
	} |
	InputsList {
		nodeList :: [Identifier]
	} |
	OutputsList {
		nodeList :: [Identifier]
	} |
	FaultDescription {
		fault :: Fault,
		tests :: TestVectors
	} deriving (Ord, Eq)

type TestVectors = [TestVector]
type TestVector = B.ByteString

instance Show Tests where
	show (Tests blocks) = concat $ map show blocks

instance Show TextBlock where
	show (Comment str) = "*" ++ str ++ "\n"
	show (InputsList ins) = showNodesList inputsMessage ins
	show (OutputsList outs) = showNodesList outputsMessage outs
	show (FaultDescription f ts) = (show f) ++ "\n" ++ (concat $ mapIndex showTest ts)

showNodesList :: String -> [Identifier] -> String
showNodesList description nodes = "* " ++ description ++ "\n  " ++
	(join " " $ map show nodes) ++ "\n"

showTest :: Int -> B.ByteString -> String
showTest i x = "      " ++ (show (i + 1)) ++ ": " ++ (B.unpack x) ++ "\n"

inputsMessage :: String
inputsMessage = "Primary inputs :"

outputsMessage :: String
outputsMessage = "Primary outputs:"