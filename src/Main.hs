module Main where

import System.Environment
import System.Cmd

import Ouroboros.Bench
import Ouroboros.Scheme
import Ouroboros.Graphviz

data Parameters = Parameters {
	sourceFile :: String,
	generateGraph :: Bool
}

main :: IO()
main = do
    [srcFile] <- getArgs
    file1 <- readFile (srcFile ++ ".bench")

    let bench1 = parseFile file1
    let scheme1 = programToScheme bench1
    let graph1 = graphFromScheme scheme1
    let scheme2 = deafenStateOutputs $ removeRecursions scheme1
    let graph2 = graphFromScheme scheme2
    let bench2 = programFromScheme scheme2
    let file2 = show bench2

    writeFile (srcFile ++ ".dot") $ show graph1
    writeFile (srcFile ++ ".out.dot") $ show graph2
    writeFile (srcFile ++ ".out.bench") $ show file2

    drawGraph srcFile graph1
    drawGraph (srcFile ++ ".out") graph2

--drawGraph :: String -> Graph -> IO ()
drawGraph filename graph = do
	system $ "dot -Tpng " ++ filename ++ ".dot -o " ++ filename ++ ".png"
	return ()

{-
reprint :: String -> String
reprint = 	show . 
			programFromScheme . 
			deafenStateOutputs . 
			removeRecursions . 
			programToScheme . 
			parseFile

getParameters :: [String] -> IO Parameters
getParameters args = putStrLn usage >> error "Wrong usage."

usage :: String
usage = "Usage: Ouroboros filename[.bench] [-g | --graph]\n" ++
		"-g | --graph\t Generate graph for specified and generated\n" ++
		"\t\t scheme using GraphViz. Graph in dot language will be\n" ++
		"\t\t stored in filename.dot file, graphics will be stored\n" ++
		"\t\t in filename.png file."

		-}