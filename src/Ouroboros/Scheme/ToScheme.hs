module Ouroboros.Scheme.ToScheme (
    programToScheme
) where

import Ouroboros.Scheme.Definition
import qualified Ouroboros.Bench.Language as AST
import qualified Data.Set as Set
import Data.List

programToScheme :: AST.Program -> Scheme
programToScheme program = 	if validateProgram program then
								Scheme {
									name 			= getName program,
									bindings 		= getBindings program,
									nodeDefinitions = getNodeDefinitions program,
									primaryIOs		= 	map nodeName $ 
														filter (\def -> nodeType def == OUTPUT || 
																		nodeType def == INPUT) $ 
														getNodeDefinitions program
								}
							else
								error "Program is not valid."
							where
								getName (AST.Program lines) = getComment $ 
															  head $ 
															  filter AST.isCommentLine lines
								getComment (AST.Comment str) = str
                                                      

validateProgram :: AST.Program -> Bool
validateProgram program = validateBindings $ getBindings program

getBindings :: AST.Program -> [Binding]
getBindings (AST.Program lines) = (nub getBindingsFromIOs) ++
								  (nub getBindingsFromDefs)
	where
		commands = map AST.getCommand $ filter AST.isCodeLine lines
		getIO (AST.Input varId) = (inputId, varId)
		getIO (AST.Output varId) = (varId, outputId)
		getBindingsFromIOs = map getIO $ filter (not . AST.isDefinition) commands
		defs = map AST.getDefinition $ filter AST.isDefinition commands
		getDef def = map (\x -> (x, fst def)) $ getBindedVars $ snd def
		getBindingsFromDefs = collapse $ map getDef defs
		collapse = foldl1 (++)

getBindedVars :: AST.Expression -> [Identifier]
getBindedVars (AST.AND   a b) = [a, b]
getBindedVars (AST.NAND  a b) = [a, b]
getBindedVars (AST.OR    a b) = [a, b]
getBindedVars (AST.NOR   a b) = [a, b]
getBindedVars (AST.XOR   a b) = [a, b]
getBindedVars (AST.BUF   a) = [a]
getBindedVars (AST.NOT   a) = [a]
getBindedVars (AST.DELAY a) = [a]

validateBindings :: [Binding] -> Bool
validateBindings bindings = result
	where
		result = iter bindings Set.empty Set.empty Set.empty

		iter :: [Binding] -> Set.Set Identifier -> Set.Set Identifier -> Set.Set Identifier -> Bool
		iter [] vars inVars outVars = all id $ Set.elems $ Set.map bothInAndOut vars
			where
					bothInAndOut v = 	if (v == inputId) || (v == outputId) then
											True
										else if not (Set.member v inVars) then
											error $ "Unspecified node: " ++ show v 
										else if not (Set.member v outVars) then
											error $ "Unknown node: " ++ show v
										else
											True

		iter (b : bs) vars inVars outVars = iter bs (Set.insert (fst b) (Set.insert (snd b) vars))
													(Set.insert (fst b) inVars)
													(Set.insert (snd b) outVars)
			
getNodeDefinitions :: AST.Program -> [NodeDefinition]
getNodeDefinitions (AST.Program lines) = ios ++ defs
	where
		commands = map AST.getCommand $ filter AST.isCodeLine lines
		getIO (AST.Input varId) = NodeDefinition varId INPUT
		getIO (AST.Output varId) = NodeDefinition varId OUTPUT
		ios = map getIO $ filter (not . AST.isDefinition) commands
		defs = map (nodeDef . AST.getDefinition) $ filter AST.isDefinition commands
		nodeDef (varId, (AST.AND _ _))  = NodeDefinition varId AND
		nodeDef (varId, (AST.NAND _ _)) = NodeDefinition varId NAND
		nodeDef (varId, (AST.OR _ _))   = NodeDefinition varId OR
		nodeDef (varId, (AST.NOR _ _))  = NodeDefinition varId NOR
		nodeDef (varId, (AST.XOR _ _))  = NodeDefinition varId XOR
		nodeDef (varId, (AST.NOT   _))  = NodeDefinition varId NOT
		nodeDef (varId, (AST.BUF   _))  = NodeDefinition varId BUF
		nodeDef (varId, (AST.DELAY _))  = NodeDefinition varId DELAY