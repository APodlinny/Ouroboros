module Ouroboros.Graphviz (
	graphFromScheme
) where

import Ouroboros.Graphviz.Language
import qualified Ouroboros.Scheme as S
import System.IO.Unsafe

graphFromScheme :: S.Scheme -> Graph
graphFromScheme scheme = Graph {
	name = S.name scheme,
	bindings = map (toGraphBinding scheme) $ S.bindings scheme
}

toGraphBinding :: S.Scheme -> S.Binding -> Binding
toGraphBinding scheme (from, to) = (from', to')
	where
		from' = (validName from) ++ "\\n" ++ (typeFor from)
		to'   = (validName to) ++ "\\n" ++ (typeFor to)

		validName = map validChar . S.str
		validChar x = 
			if x == '[' then
				'<'
			else if x == ']' then
				'>'
			else
				x

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

logger :: (a -> String) -> a -> a
logger f x = unsafePerformIO $ do
	putStrLn $ f x
	return x