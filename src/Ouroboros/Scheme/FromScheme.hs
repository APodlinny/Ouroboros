module Ouroboros.Scheme.FromScheme (
    programFromScheme
) where

import Ouroboros.Scheme.Definition
import qualified Ouroboros.Language as AST

programFromScheme :: Scheme -> AST.Program
programFromScheme (Scheme schemeName matrix nodes) = AST.Program textLines
	where
		textLines = (AST.Comment schemeName) : codeLines
		codeLines = map makeCodeLine commands
		makeCodeLine command = AST.Code command AST.Empty
		commands = getCommands matrix nodes
        
getCommands :: ConnectionMatrix -> [Node] -> [AST.Command]
getCommands matrix nodes = getInputs matrix nodes :
						   getOutputs matrix nodes :
						   getDefinitions matrix nodes
	where
		getInputs = undefined
		getOutputs = undefined
        
getDefinitions :: ConnectionMatrix -> [Node] -> [AST.Command]
getDefinitions = undefined

