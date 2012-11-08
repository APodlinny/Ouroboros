{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import System.Environment
import System.Cmd
import System.Console.GetOpt
import System.Exit
import qualified System.IO.Strict as Strict

import Ouroboros.Common
import Ouroboros.Bench
import Ouroboros.Scheme
import Ouroboros.Graphviz
import Ouroboros.Fault
import Ouroboros.Test

data Parameters = Parameters {
	srcFile :: String,
    dstFile :: String,
    fltFile :: String,
	generateGraph :: Bool,
    numCopies :: Int
} deriving (Show)

defaultParameters :: Parameters
defaultParameters = Parameters {
    srcFile = "",
    fltFile = "",
    generateGraph = False,
    numCopies = 1
}

options :: [OptDescr (Parameters -> Parameters)]
options = [
    Option ['g'] ["graph"]  (NoArg (\p -> p { generateGraph = True })) "Draw scheme graph or not (GraphViz required).",
    Option ['i'] ["input"]  (ReqArg (\x p -> p { srcFile = x }) "File") "Input file",
    Option ['f'] ["fault"]  (ReqArg (\x p -> p { fltFile = x }) "File") "Fault file",
    Option ['c'] ["copies"] (ReqArg (\x p -> p { numCopies = read x }) "Integer") "How many copies of the specified scheme to make."]

main :: IO ()
main = do
    args <- getArgs
    let (flags, nonOpts, errors) = getOpt RequireOrder options args
    let parameters = foldr ($) defaultParameters flags
    if (length flags == 0) || (length errors /= 0) then do
        putStrLn $ concat errors
        usage
    else return ()

    checkParameters parameters

    let srcName = srcFile parameters

    benchSrc <- readFile srcName
    let schemeSrc = programToScheme $ parseBenchFile benchSrc 
    let schemeDst = deafenStateOutputs $ copySchemeTimes (numCopies parameters) $ removeRecursions schemeSrc
    
    writeFile ((nameWithoutExtention srcName) ++ ".out") $ 
        show $ 
        programFromScheme schemeDst

    faultsStr <- Strict.readFile $ fltFile parameters
    let faults = parseFaultsFile faultsStr
    let additionalFaults = addSimilarFaults schemeDst faults
    print additionalFaults

    runAtalanta ((nameWithoutExtention srcName) ++ ".out") (fltFile parameters)
    testFile <- Strict.readFile $ (nameWithoutExtention srcName) ++ ".test"
    let tests = parseTestsFile testFile
    let packed = repackTests schemeDst tests
    writeFile ((nameWithoutExtention srcName) ++ ".test") $ show packed

checkParameters :: Parameters -> IO ()
checkParameters p = 
    if (srcFile p == "") || (fltFile p == "") then do
        usage
    else
        return ()

usage :: IO ()
usage = do
    putStrLn $ usageInfo "Ouroboros -i INPUT -o OUTPUT -f FAULT [-g]" options
    exitWith ExitSuccess

nameWithoutExtention :: String -> String
nameWithoutExtention = 
    join "." .
    init .
    split '.'

runAtalanta :: String -> String -> IO ()
runAtalanta bench fault = do
    atalantaResult <- system $ "atalanta " ++ bench ++ " -A -f " ++ fault ++ " > NUL"

    case atalantaResult of
        ExitFailure 1 -> do
            putStrLn "\nSomething went wrong."
            exitWith $ ExitFailure 1
        ExitFailure _ -> return ()
        ExitSuccess -> return ()

{-
main :: IO()
main = do
    [srcFile] <- getArgs
    file1 <- readFile (srcFile ++ ".bench")

    let bench1 = parseFile file1
    let scheme1 = programToScheme bench1
    let graph1 = graphFromScheme scheme1
    let scheme2 = deafenStateOutputs $ copySchemeTimes 3 $ removeRecursions scheme1
    let graph2 = graphFromScheme scheme2
    let bench2 = programFromScheme scheme2
    let file2 = show bench2

    print $ similarNames scheme2

    writeFile (srcFile ++ ".dot") $ show graph1
    writeFile (srcFile ++ ".out.dot") $ show graph2
    writeFile (srcFile ++ ".out.bench") $ file2

    drawGraph srcFile
    drawGraph (srcFile ++ ".out")

drawGraph :: String -> IO ()
drawGraph filename = do
	system $ "dot -Tpng " ++ filename ++ ".dot -o " ++ filename ++ ".png"
	return ()
-}