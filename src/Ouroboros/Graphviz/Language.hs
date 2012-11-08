module Ouroboros.Graphviz.Language (
	Graph(..),
	Binding
) where

type Binding = (String, String)

data Graph = Graph {
	name :: String,
	bindings :: [Binding],
	sameColored :: [(String, [String])]
}

instance Show Graph where
	show g = "digraph " ++ (show $ name g) ++ " {\n" ++ 
		"rankdir = LR\n" ++ 
		(setColors $ sameColored g) ++
		(showBinds $ bindings g) ++ "}"

showBinds :: [Binding] -> String
showBinds bs = joinLines (map showBind bs)

showBind :: Binding -> String
showBind (from, to) = "\"" ++ from ++ "\" -> \"" ++ to ++ "\""

setColors :: [(String, [String])] -> String
setColors colorSettings = concat $ map setColor colorSettings

setColor :: (String, [String]) -> String
setColor (color, nodes) = joinLines $ map (setColorFor color) nodes

setColorFor :: String -> String -> String
setColorFor color node = "\"" ++ node ++ "\"" ++ " [color=\"" ++ color ++ "\", style=filled]"

joinLines :: [String] -> String
joinLines lines = (foldl1 (\a b -> a ++ ";\n" ++ b) lines) ++ ";\n"