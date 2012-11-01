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
    let scheme2 = copySchemeTimes 7 $ removeRecursions scheme1
    let graph2 = graphFromScheme scheme2
    let bench2 = programFromScheme scheme2
    let file2 = show bench2

    writeFile (srcFile ++ ".dot") $ show graph1
    writeFile (srcFile ++ ".out.dot") $ show graph2
    writeFile (srcFile ++ ".out.bench") $ show file2

    drawGraph srcFile
    drawGraph (srcFile ++ ".out")

drawGraph :: String -> IO ()
drawGraph filename = do
	system $ "dot -Tpdf " ++ filename ++ ".dot -o " ++ filename ++ ".pdf"
	return ()