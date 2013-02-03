module Ouroboros.Fault.Language (
	AST.Identifier(..),
	Faults(..),
	Fault(..),
	FaultType(..)
) where

import qualified Ouroboros.Bench.Language as AST

type Identifier = AST.Identifier

data Faults = Faults {
	faults :: [Fault]
}

data Fault = 
	Node { 
		node :: Identifier, 
		faultType :: FaultType 
	} |
	Junction { 
		nodes :: (Identifier, Identifier), 
		faultType :: FaultType 
	} deriving (Eq, Ord)

data FaultType = AtZero | AtOne deriving (Eq, Ord)

instance Show FaultType where
	show AtZero = "/0"
	show AtOne  = "/1"

instance Show Fault where
	show (Node { node = n, faultType = f }) = (show n) ++ " " ++ (show f)
	show (Junction { nodes = (from, to), faultType = f }) = (show from) ++ "->" ++ (show to) ++ " " ++ (show f)

instance Show Faults where
	show fs = (join "\n" $ map show $ faults fs) ++ "\n"
		where
			join sep = foldl1 (\a b -> a ++ sep ++ b)