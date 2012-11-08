module Ouroboros.Graphviz (
	graphFromScheme
) where

import Ouroboros.Graphviz.Language
import qualified Ouroboros.Scheme as S

graphFromScheme :: S.Scheme -> Graph
graphFromScheme scheme = Graph {
	name = S.name scheme,

	bindings = map (toGraphBinding scheme) $ 
		S.bindings scheme,

	sameColored = map (setSameColored scheme) []
}

toGraphBinding :: S.Scheme -> S.Binding -> Binding
toGraphBinding scheme (from, to) = (from', to')
	where
		from' = toGraphName scheme from
		to'   = toGraphName scheme to

toGraphName :: S.Scheme -> S.Identifier -> String
toGraphName scheme nodeId = (validName nodeId) ++ "\\n" ++
							(typeFor scheme nodeId)

setSameColored :: S.Scheme -> [S.Identifier] -> (String, [String])
setSameColored scheme nodes = ("red", map (toGraphName scheme) nodes)

validName :: S.Identifier -> String
validName = map validChar . S.str

validChar :: Char -> Char
validChar x = 
	if x == '[' then
		'<'
	else if x == ']' then
		'>'
	else
		x

typeFor :: S.Scheme -> S.Identifier -> String
typeFor scheme nodeId = 
	if nodeId == S.inputId then
		"INPUT"
	else if nodeId == S.outputId then
		"OUTPUT"
	else
		case typeList of
			[] -> "NOTYPE"
			(x:_) -> show $ S.nodeType x
	where
		typeList = 
			filter (\x -> S.nodeType x /= S.OUTPUT) $
			filter (\x -> S.nodeName x == nodeId) $ 
			S.nodeDefinitions scheme