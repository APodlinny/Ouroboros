module Ouroboros.Graphviz (
	graphFromScheme
) where

import Ouroboros.Graphviz.Language
import qualified Ouroboros.Scheme as S

graphFromScheme :: S.Scheme -> Graph
graphFromScheme scheme = Graph {
	name = S.name scheme,
	bindings = map (toGraphBinding scheme) $ S.bindings scheme
}

toGraphBinding :: S.Scheme -> S.Binding -> Binding
toGraphBinding scheme (from, to) = (from', to')
	where
		from' = (validate from) ++ "\\n" ++ (typeFor from)
		to'   = (validate to) ++ "\\n" ++ (typeFor to)

		validate nodeId = filter (\x -> (x /= '[') && (x /= ']')) $ S.str nodeId

		typeFor nodeId = 
			if nodeId == S.inputId then
				"INPUT"
			else if nodeId == S.outputId then
				"OUTPUT"
			else
				show $
				S.nodeType $ 
				head $
				filter (\x -> S.nodeType x /= S.OUTPUT) $
				filter (\x -> S.nodeName x == nodeId) $ 
				S.nodeDefinitions scheme