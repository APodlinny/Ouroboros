module Ouroboros.Graphviz.Language (
	Graph(..),
	Binding
) where

type Binding = (String, String)

data Graph = Graph {
	name :: String,
	bindings :: [Binding]
}

instance Show Graph where
	show g = "digraph " ++ (show $ name g) ++ " {\n" ++ (showBinds $ bindings g) ++ "}"

showBinds :: [Binding] -> String
showBinds bs = joinLines (map showBind bs) ++ ";\n"
	where
		joinLines = foldl1 (\a b -> a ++ ";\n" ++ b)

showBind :: Binding -> String
showBind (from, to) = "\"" ++ from ++ "\" -> \"" ++ to ++ "\""