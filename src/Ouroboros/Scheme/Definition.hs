module Ouroboros.Scheme.Definition (..) where
{-    Scheme(..),
    ConnectionMatrix(..),
    Connection(..),
    Identifier(..),
    NodeType(..),
    Node(..),
    inputId,
    outputId,
    indexOf,
    matrixAt,
    repeatN,
    repeatMatrixN, 
    setConnectionIJ,
    setConnection,
    applySetter,
    oppositeConnection-}
--) where

import qualified Ouroboros.Language as AST

data Scheme = Scheme String ConnectionMatrix [Node]
type ConnectionMatrix = [[Connection]]
data Connection = Input | Output | NotConnected
type Identifier = AST.Identifier
data NodeType = AND | NAND | OR | NOR | XOR | NOT | BUF | DELAY | INPUT | OUTPUT
data Node = Node {
	identifier :: Identifier,
	nodeType :: NodeType
} 

instance Eq Node where
	a == b = (identifier a) == (identifier b)
			
inputId  = AST.Identifier "__input__"
outputId = AST.Identifier "__output__"

indexOf :: Eq a => a -> [a] -> Int
indexOf n list = iter list 0 where
	iter [x] counter = if x == n then
						 counter
					   else
						 error "indexOf: element is not found."
	iter (x:xs) counter = if x == n then
							counter
						  else
							iter xs (counter + 1)
							
matrixAt :: Int -> Int -> ConnectionMatrix -> Connection
matrixAt i j m = ((m !! i) !! j)

repeatN :: Int -> a -> [a]
repeatN count elem = take count $ repeat elem

repeatMatrixN :: Int -> a -> [[a]]
repeatMatrixN count elem = repeatN count $ repeatN count elem

setConnectionIJ :: Int -> Int -> Connection -> 
					ConnectionMatrix -> ConnectionMatrix
setConnectionIJ i j connection matrix = map (\i -> map (setter i) indices) indices
	where
		indices = [0 .. length matrix - 1]
		predicate x y = (i, j) == (y, x)
		setter x y = if predicate x y then
						connection
					 else
						matrixAt x y matrix
                        
setConnection :: [Node] -> 
                 (Identifier, Identifier, Connection) -> 
                 ConnectionMatrix -> ConnectionMatrix
setConnection nodes (nodeA, nodeB, connection) matrix = result
	where
		nodeIndexA = indexOf (Node nodeA AND) nodes
		nodeIndexB = indexOf (Node nodeB AND) nodes
		connection' = oppositeConnection connection
		setted = setConnectionIJ nodeIndexA nodeIndexB connection matrix
		result = setConnectionIJ nodeIndexB nodeIndexA connection' setted

applySetter :: ConnectionMatrix -> 
			   [ConnectionMatrix -> ConnectionMatrix] -> 
			   ConnectionMatrix
applySetter matrix [] = matrix
applySetter matrix (f:fs) = applySetter (f matrix) fs
		
oppositeConnection :: Connection -> Connection
oppositeConnection NotConnected = NotConnected
oppositeConnection Input = Output
oppositeConnection Output = Input
