module Ouroboros.Parser (
    parseFile
) where

import Ouroboros.Language
import Text.ParserCombinators.Parsec

handleError :: Either ParseError Program -> Program
handleError (Left err) = error $ "Error: " ++ (show err)
handleError (Right program) = program

parseFile :: String -> Program
parseFile str = handleError $ parse program "" str

program :: Parser Program
program = do
    lines <- textLine `endBy` eol
    return $ Program lines

textLine :: Parser TextLine
textLine = codeLine <|>
           (try $ commentLine :: Parser TextLine) <|>
           emptyLine

emptyLine :: Parser TextLine
emptyLine = do
    many $ char ' '
    return Empty

codeLine :: Parser TextLine
codeLine = do
    code <- command
    result <- (commentLine >>= (\c -> return $ Code code c)) <|>
        (return $ Code code (Comment ""))
    return result
    
commentLine :: Parser TextLine
commentLine = do
    many $ char ' '
    char '#'
    comment <- many $ noneOf "\r\n"
    return $ Comment comment

command :: Parser Command
command = input <|>
          output <|>
          definition <|>
          fail "invalid operation"

input :: Parser Command
input = do
    operation <- string "input" <|>
                 string "INPUT"
    char '('
    arg <- identifier
    char ')'
    return $ Input arg
    
output :: Parser Command
output = do
    operation <- string "output" <|>
                 string "OUTPUT"
    char '('
    arg <- identifier
    char ')'
    return $ Output arg

definition :: Parser Command
definition = do
    var <- identifier
    many $ char ' '
    char '='
    many $ char ' '
    expr <- expression
    return $ Definition var expr

expression :: Parser Expression
expression = andExpr <|>
             orExpr <|>
             nandExpr <|>
             norExpr <|>
             notExpr <|>
             xorExpr <|>
             bufExpr <|>
             delayExpr <|>
             (fail "invalid operation")
             
andExpr :: Parser Expression
andExpr = do
    operation <- string "and" <|>
                 string "AND"
    char '('
    arg1 <- identifier
    char ','
    many $ char ' '
    arg2 <- identifier
    char ')'
    return $ AND arg1 arg2

orExpr :: Parser Expression    
orExpr = do
    operation <- string "or" <|>
                 string "OR"
    char '('
    arg1 <- identifier
    char ','
    many $ char ' '
    arg2 <- identifier
    char ')'
    return $ OR arg1 arg2

nandExpr :: Parser Expression    
nandExpr = do
    operation <- string "nand" <|>
                 string "NAND"
    char '('
    arg1 <- identifier
    char ','
    many $ char ' '
    arg2 <- identifier
    char ')'
    return $ NAND arg1 arg2

norExpr :: Parser Expression    
norExpr = do
    operation <- string "nor" <|>
                 string "NOR"
    char '('
    arg1 <- identifier
    char ','
    many $ char ' '
    arg2 <- identifier
    char ')'
    return $ NOR arg1 arg2

notExpr :: Parser Expression    
notExpr = do
    operation <- string "not" <|>
                 string "NOT"
    char '('
    arg <- identifier
    char ')'
    return $ NOT arg

xorExpr :: Parser Expression    
xorExpr = do
    operation <- string "xor" <|>
                 string "XOR"
    char '('
    arg1 <- identifier
    char ','
    many $ char ' '
    arg2 <- identifier
    char ')'
    return $ XOR arg1 arg2

delayExpr :: Parser Expression    
delayExpr = do
    operation <- string "dff" <|>
                 string "DFF"
    char '('
    arg <- identifier
    char ')'
    return $ DELAY arg

bufExpr :: Parser Expression    
bufExpr = do
    operation <- (try $ string "buff" :: Parser String) <|> 
        (try $ string "BUFF" :: Parser String) <|> 
        string "buf" <|> 
        string "BUF"
    char '('
    arg <- identifier
    char ')'
    return $ BUF arg

eol :: Parser String
eol = try (string "\n\r") <|> 
      try (string "\r\n") <|> 
      string "\n" <|> 
      string "\r" <?> 
      "end of line"

identifier :: Parser Identifier
identifier = do
    name <- many1 $ oneOf validChars
    return $ Identifier name
