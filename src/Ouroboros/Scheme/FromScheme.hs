module Ouroboros.Scheme.FromScheme (
    programFromScheme
) where

import qualified Ouroboros.Scheme.Definition as Scheme
import Ouroboros.Language

programFromScheme :: Scheme.Scheme -> Program
programFromScheme scheme = Program textLines
	where
		textLines = (Comment $ Scheme.name scheme) :
					((primaryIOsComments scheme) ++ codeLines)
		codeLines = map (\c -> Code c Empty) commands
		commands  = (getIOPorts scheme) ++
					(getDefinitions scheme)

getIOPorts :: Scheme.Scheme -> [Command]
getIOPorts scheme = result
	where
		cmds = Scheme.nodeDefinitions scheme
		inputs = filter (\d -> (Scheme.nodeType d) == Scheme.INPUT) cmds
		outputs = filter (\d -> (Scheme.nodeType d) == Scheme.OUTPUT) cmds
		toCommand (Scheme.NodeDefinition nodeId Scheme.INPUT) = Input nodeId
		toCommand (Scheme.NodeDefinition nodeId Scheme.OUTPUT) = Output nodeId
		result = (map toCommand inputs) ++ (map toCommand outputs)

getDefinitions :: Scheme.Scheme -> [Command]
getDefinitions scheme = result
	where
		cmds = Scheme.nodeDefinitions scheme
		defs = filter (\d -> (Scheme.nodeType d /= Scheme.INPUT) &&
							 (Scheme.nodeType d /= Scheme.OUTPUT)) cmds

		toDefinition nodeDef = Definition (Scheme.nodeName nodeDef) $ 
			expressionFor (Scheme.bindings scheme) nodeDef
		
		result = map toDefinition defs

expressionFor :: [Scheme.Binding] -> Scheme.NodeDefinition -> Expression
expressionFor bindings nodeDef = expr
	where
		args = bindedNodes bindings $ Scheme.nodeName nodeDef
		expr = makeExpression (Scheme.nodeType nodeDef) args

bindedNodes :: [Scheme.Binding] -> Identifier -> [Identifier]
bindedNodes bindings nodeId = bindedVars
	where
		varBindings = filter (\(from, to) -> to == nodeId) bindings
		bindedVars = map fst varBindings

makeExpression :: Scheme.NodeType -> [Identifier] -> Expression
makeExpression Scheme.AND  [a, b] = AND  a b
makeExpression Scheme.NAND [a, b] = NAND a b
makeExpression Scheme.OR   [a, b] = OR   a b
makeExpression Scheme.NOR  [a, b] = NOR  a b
makeExpression Scheme.XOR  [a, b] = XOR  a b
makeExpression Scheme.BUF     [a] = BUF  a
makeExpression Scheme.NOT     [a] = NOT  a
makeExpression Scheme.DELAY   [a] = DELAY  a

primaryIOsComments :: Scheme.Scheme -> [TextLine]
primaryIOsComments scheme = Comment "Primary IOs:" : ios ++ [Empty]
	where
		schemeIOs = Scheme.primaryIOs scheme
		ios = map (\(Identifier str) -> Comment str) schemeIOs