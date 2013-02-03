{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import System.Environment
import System.Cmd
import System.Console.GetOpt
import System.Exit
import qualified System.IO.Strict as Strict
import Control.Exception
import Data.Maybe

import Ouroboros.Common
import Ouroboros.Bench
import Ouroboros.Scheme
import Ouroboros.Graphviz
import Ouroboros.Fault
import Ouroboros.Test

data Parameters = Parameters {
	srcFile :: String,
    fltFile :: String,
    maxCopies :: Int
} deriving (Show)

defaultParameters :: Parameters
defaultParameters = Parameters {
    srcFile = "",
    fltFile = "",
    maxCopies = 10
}

options :: [OptDescr (Parameters -> Parameters)]
options = [
    Option ['i'] ["input"]  (ReqArg (\x p -> p { srcFile = x }) "File") "Input file.",
    Option ['f'] ["fault"]  (ReqArg (\x p -> p { fltFile = x }) "File") "Fault file.",
    Option ['c'] ["copies"] (ReqArg (\x p -> p { maxCopies = read x }) "Integer") "How many copies of the specified scheme to make. Dafault value is 10."]

main :: IO ()
main = do
    parameters <- getParameters >>= checkParameters

    let srcName = srcFile parameters

    putStrLn "Parsing input file"
    benchSrc <- readFile srcName >>= (return . parseBenchFile)
    schemeSrc <- evaluate $ programToScheme benchSrc
    
    putStrLn "Removing recursions"
    noRecScheme <- evaluate $ removeRecursions schemeSrc

    putStrLn "Reading faults file"
    faultsFromFile <- readFile (fltFile parameters) >>= (return . parseFaultsFile)

    analisysResult <- analiseScheme parameters noRecScheme (maxCopies parameters) (faults faultsFromFile)

    putStrLn "Writing test file"
    writeFile ((nameWithoutExtention srcName) ++ ".test") $ show analisysResult
    putStrLn "Exiting"

analiseScheme :: Parameters -> Scheme -> Int -> [Fault] -> IO Tests
analiseScheme parameters noRecScheme maxCopies faults = do
    iterationResult <- iteration 1 faults (Tests [])
    
    let testedFaults = map fault $ filter isFaultBlock $ blocks iterationResult
    let untestedFaults = filter (not . (`elem` testedFaults)) faults
    let emptyTests = map (\x -> FaultDescription x []) untestedFaults
    let fullTests = addTextBlocks iterationResult emptyTests
    
    return fullTests

    where
        iteration :: Int -> [Fault] -> Tests -> IO Tests
        iteration copies faults lastTests = do
            putStrLn $ "\n\tTransforming scheme (" ++ (show copies) ++ " copy)"
            schemeCopied <- evaluate $ copySchemeTimes copies noRecScheme
            schemeDst <- evaluate $ deafenStateOutputs schemeCopied

            let benchFile = (nameWithoutExtention (srcFile parameters)) ++ ".out"
            let faultFile = (fltFile parameters) ++ ".tmp"
            let testFile = (nameWithoutExtention (srcFile parameters)) ++ ".test"

            putStrLn "\tWriting transformed scheme"
            writeFile benchFile $ 
                show $ 
                programFromScheme schemeDst

            let (Faults faultCopies) = addSimilarFaults schemeDst (Faults faults)
            let faultsWithCopies = faultCopies

            putStrLn "\tWriting faults file"
            writeFile faultFile $
                show $
                Faults faultsWithCopies

            putStrLn "\tAnalising transformed scheme with Atalanta"
            runAtalanta benchFile faultFile

            putStrLn "\tAnalising produced tests"
            tests <- Strict.readFile testFile >>= (return . parseTestsFile)
            
            let repackedTests = repackTests schemeDst tests

            let testedFaults = getTestedFaults schemeDst repackedTests
            let untestedFaults = getUntestedFaults schemeDst repackedTests
            
            onlyPackedTests <- evaluate $ 
                map fromJust $ 
                filter isJust $ 
                map (getTestByFault repackedTests) testedFaults

            let additionalTests = removeUnnecessaryInfo schemeDst $ addTextBlocks (removeTestsInfo tests) onlyPackedTests
            
            let newTests = addTextBlocks lastTests (blocks $ additionalTests)
            
            if length untestedFaults == 0 then do
                putStrLn "\tAnalisys is finished, forming output test file"
                return newTests
            else do
                putStrLn "\tUntested faults at current step:"
                putStrLn $ join ", " $ map show untestedFaults

                if copies < maxCopies then
                    iteration (copies + 1) untestedFaults newTests
                else do
                    putStrLn "\tCan't generate tests for remained faults."
                    return newTests


checkParameters :: Parameters -> IO Parameters
checkParameters p = do
    if (srcFile p == "") || (fltFile p == "") then do
        usage
    else return ()

    return p

getParameters :: IO Parameters
getParameters = do
    args <- getArgs
    
    let (flags, nonOpts, errors) = getOpt RequireOrder options args
    let parameters = foldr ($) defaultParameters flags
    
    if (length flags == 0) || (length errors /= 0) then do
        putStrLn $ concat errors
        usage
    else return () 

    return parameters

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

drawGraph :: Scheme -> String -> IO ()
drawGraph scheme filename = do
    let graph = graphFromScheme scheme
    writeFile (filename ++ ".dot") $ show graph
    system $ "dot -Tpng " ++ filename ++ ".dot -o " ++ filename ++ ".png"
    return ()