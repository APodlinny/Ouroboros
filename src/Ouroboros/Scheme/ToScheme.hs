module Ouroboros.Scheme.ToScheme (
    programToScheme
) where

import Ouroboros.Scheme.Definition
import qualified Ouroboros.Language as AST

programToScheme :: AST.Program -> Scheme
programToScheme program = if not $ isProgramValid program then
						Scheme schemeName matrix nodes
					else
						error "Program is not valid."
					where
						schemeName = getName program
						nodes = getNodes program
						matrix = createMatrix program nodes
						getComment (AST.Comment str) = str
						getName (AST.Program lines) = getComment $ 
													  head $ 
													  filter AST.isCommentLine lines
                                                      
-- TODO: add program validation
isProgramValid :: AST.Program -> Bool
isProgramValid program = True

createMatrix :: AST.Program -> [Node] -> ConnectionMatrix
createMatrix program nodes = settedMatrix 
	where
		emptyMatrix = repeatMatrixN nodeCount NotConnected
		nodeCount = length nodes
		connections = connectionsFromProgram program
		settedMatrix = applySetter emptyMatrix setters
		setters = map (\conn matrix ->
						setConnection nodes conn matrix)
					  connections

getNodes :: AST.Program -> [Node]
getNodes program = [Node inputId INPUT,
					Node outputId OUTPUT] ++ 
				   (map nodeFromDefinition $  
					getDefinitions program)

getDefinitions :: AST.Program -> [(Identifier, AST.Expression)]
getDefinitions (AST.Program lines) = map AST.getDefinition $ 
                                        filter AST.isDefinition $ 
                                        map AST.getCommand $ 
                                        filter AST.isCodeLine lines

connectionsFromProgram :: AST.Program -> [(Identifier, Identifier, Connection)]
connectionsFromProgram program = join $ map connectionFromCommand $ cmds program
	where
		join = foldl1 (++)
		cmds (AST.Program lines) = map AST.getCommand $ filter AST.isCodeLine lines
										
connectionFromCommand :: AST.Command -> [(Identifier, Identifier, Connection)]
connectionFromCommand (AST.Input id) = [(id, inputId, Input)]
connectionFromCommand (AST.Output id) = [(outputId, id, Output)]
connectionFromCommand (AST.Definition var expr) = connectionFromDefinition (var, expr)
										
connectionFromDefinition :: (Identifier, AST.Expression) -> [(Identifier, Identifier, Connection)]
connectionFromDefinition (var, expr) = map (\node -> (var, node, Input)) connectedNodes
	where 
		connectedNodes = getExpressionIds expr
										
getExpressionIds :: AST.Expression -> [Identifier]
getExpressionIds (AST.AND a b) = [a, b]
getExpressionIds (AST.NAND a b) = [a, b]
getExpressionIds (AST.OR a b) = [a, b]
getExpressionIds (AST.NOR a b) = [a, b]
getExpressionIds (AST.XOR a b) = [a, b]
getExpressionIds (AST.BUF a) = [a]
getExpressionIds (AST.NOT a) = [a]
getExpressionIds (AST.DELAY a) = [a]
										
nodeFromDefinition :: (Identifier, AST.Expression) -> Node
nodeFromDefinition (id, (AST.AND  _ _)) = Node id AND
nodeFromDefinition (id, (AST.NAND _ _)) = Node id NAND
nodeFromDefinition (id, (AST.OR   _ _)) = Node id OR
nodeFromDefinition (id, (AST.NOR  _ _)) = Node id NOR
nodeFromDefinition (id, (AST.XOR  _ _)) = Node id XOR
nodeFromDefinition (id, (AST.BUF    _)) = Node id BUF
nodeFromDefinition (id, (AST.NOT    _)) = Node id NOT
nodeFromDefinition (id, (AST.DELAY  _)) = Node id DELAY
										
getConnections :: [Node] -> Node -> ConnectionMatrix -> [Connection]
getConnections nodes node matrix = matrix !! nodeIndex where
	nodeIndex = indexOf node nodes
	
getConnection :: [Node] -> Node -> Node -> ConnectionMatrix -> Connection
getConnection nodes nodeA nodeB = matrixAt nodeIndexA nodeIndexB where
	nodeIndexA = indexOf nodeA nodes
	nodeIndexB = indexOf nodeB nodes