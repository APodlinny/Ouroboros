module Ouroboros.Scheme.Definition (
	Scheme(..),
	Identifier(..),
	NodeDefinition(..),
	NodeType(..),
	Binding(..),
	inputId,
	outputId
) where

import qualified Ouroboros.Language as AST

data Scheme = Scheme {
	name :: String,
	bindings :: [Binding],
	nodeDefinitions :: [NodeDefinition]
}

data NodeDefinition = NodeDefinition {
	nodeName :: Identifier,
	nodeType :: NodeType
}

data NodeType = INPUT | OUTPUT | AND | NAND | OR | NOR | XOR | BUF | NOT | DELAY
type Binding = (Identifier, Identifier)
type Identifier = AST.Identifier

inputId = AST.Identifier "[__input__]"
outputId = AST.Identifier "[__output__]"