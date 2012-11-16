module Ouroboros.Test.Parser (
	parseTestsFile
) where

import Text.ParserCombinators.Parsec

import Ouroboros.Common
import Ouroboros.Test.Language
import Ouroboros.Bench.Parser (identifier, eol)
import Ouroboros.Fault.Parser (parseFault)

parseTestsFile :: String -> Tests
parseTestsFile str = unpackEither $ parse parseBlocks "" str

parseBlocks :: Parser Tests
parseBlocks = do
	blocks <- many1 parseBlock
	return $ Tests blocks

parseBlock :: Parser TextBlock
parseBlock = try parseInputsList <|>
			try parseOutputsList <|>
			try parseComment <|>
			parseFaultDescription <|>
			fail "unrecognized text block"

parseComment :: Parser TextBlock
parseComment = do
	char '*'
	comment <- many $ noneOf "\r\n"
	eol
	many $ oneOf " \n"
	return $ Comment comment

parseInputsList :: Parser TextBlock
parseInputsList = parseNodeList inputsMessage >>= (return . InputsList)

parseOutputsList :: Parser TextBlock
parseOutputsList = parseNodeList outputsMessage >>= (return . OutputsList)

parseNodeList :: String -> Parser [Identifier]
parseNodeList description = do
	string "* "
	string description
	eol
	--nodes <- parseList
	string "  "
	nodes <- identifier `sepEndBy1` (char ' ')
	eol
	many1 $ oneOf " \n"
	return nodes

parseList :: Parser [Identifier]
parseList = do
	lines <- parseListLine `endBy1` eol
	return $ concat lines

parseListLine :: Parser [Identifier]
parseListLine = do
	string "  "
	nodes <- identifier `sepBy` (char ' ')
	char ' '
	return nodes

parseFaultDescription :: Parser TextBlock
parseFaultDescription = do
	f <- parseFault
	eol
	ts <- many parseTestVector
	many eol
	return $ FaultDescription f ts

parseTestVector :: Parser String
parseTestVector = do
	many $ char ' '
	many1 $ oneOf ['0' .. '9']
	string ": "
	test <- many1 $ oneOf "01 x"
	eol
	return test
