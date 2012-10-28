module Main where

import System.Environment
import Ouroboros.Parser
import Ouroboros.Scheme

main :: IO()
main = do
    [fileName] <- getArgs
    text <- readFile fileName
    writeFile (fileName ++ ".out") $ reprint text

reprint :: String -> String
reprint = handleError . parseFile

handleError (Left error) = "Error: " ++ (show error)
handleError (Right program) = show program