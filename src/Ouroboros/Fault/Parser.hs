module Ouroboros.Fault.Parser (
	parseFaultsFile,
	parseFault
) where

import Text.ParserCombinators.Parsec

import Ouroboros.Common
import Ouroboros.Fault.Language
import qualified Ouroboros.Bench.Language as AST
import Ouroboros.Bench.Parser (identifier, eol)

parseFaultsFile :: String -> Faults
parseFaultsFile str = unpackEither $ parse parseFaults "" str

parseFaults :: Parser Faults
parseFaults = do
	lines <- parseFault `sepEndBy1` eol
	return $ Faults $ concat lines

parseFault :: Parser [Fault]
parseFault = 
	try parseJunction <|>
	parseNode <|>
	fail "unrecognized line"

parseJunction :: Parser [Fault]
parseJunction = do
	idA <- identifier
	many $ char ' '
	string "->"
	many $ char ' '
	idB <- identifier
	many $ char ' '
	f <- parseFaultTypes
	return $ map (Junction (idA, idB)) f

parseNode :: Parser [Fault]
parseNode = do
	idA <- identifier
	many $ char ' '
	f <- parseFaultTypes
	return $ map (Node idA) f

parseFaultTypes :: Parser [FaultType]
parseFaultTypes = parseFaultType `sepBy1` (char ' ')

parseFaultType :: Parser FaultType
parseFaultType = 
	try parseFaultAtOne <|>
	parseFaultAtZero <|>
	fail "unrecognized fault type"

parseFaultAtOne :: Parser FaultType
parseFaultAtOne = do
	string "/1"
	return AtOne

parseFaultAtZero :: Parser FaultType
parseFaultAtZero = do
	string "/0"
	return AtZero