module Ouroboros.Language (
    Program(..),
    TextLine(..),
    Command(..),
    Expression(..),
    Identifier(..),
    validChars,
    isCommentLine,
    isCodeLine,
    isEmptyLine,
    isInput,
    isOutput,
    isDefinition,
    getDefinition,
    getCommand
) where

import Data.List

-- predicates
isCommentLine (Comment _) = True
isCommentLine _ = False
isCodeLine (Code _ _) = True
isCodeLine _ = False
isEmptyLine Empty = True
isEmptyLine _ = False

isInput (Input _) = True
isInput _ = False
isOutput (Output _) = True
isOutput _ = False
isDefinition (Definition _ _) = True
isDefinition _ = False

-- extrators
getDefinition (Definition var expr) = (var, expr)
getCommand (Code command _) = command

-- AST definition
data Program = Program [TextLine]
data TextLine = Comment String | 
                Code    Command TextLine | 
                Empty

data Command = Input      Identifier |
               Output     Identifier |
               Definition Identifier Expression
               deriving Eq
               
data Expression = AND   Identifier Identifier |
                  NAND  Identifier Identifier |
                  OR    Identifier Identifier |
                  NOR   Identifier Identifier |
                  XOR   Identifier Identifier |
                  BUF   Identifier |
                  NOT   Identifier |
                  DELAY Identifier
                  deriving Eq
                  
data Identifier = Identifier { 
    str :: String 
} deriving (Eq, Ord)

instance Show Identifier where
    show (Identifier a) = if all valid a then
                              a
                          else
                              error $ "Identifier " ++ a ++ " is not valid"
        where
            valid char = isInfixOf [char] validChars --contains [char] validChars

-- instances
instance Show Expression where
    show (AND a b)  = "and(" ++ show a ++ ", " ++ show b ++ ")" --format "and({0}, {1})"  [show a, show b]
    show (NAND a b) = "nand(" ++ show a ++ ", " ++ show b ++ ")" --format "nand({0}, {1})" [show a, show b]
    show (OR a b)   = "or(" ++ show a ++ ", " ++ show b ++ ")" --format "or({0}, {1})"   [show a, show b]
    show (NOR a b)  = "nor(" ++ show a ++ ", " ++ show b ++ ")" --format "nor({0}, {1})"  [show a, show b]
    show (XOR a b)  = "xor(" ++ show a ++ ", " ++ show b ++ ")" --format "xor({0}, {1})"  [show a, show b]
    show (BUF a)    = "buf(" ++ show a ++ ")" --format "buf({0})"       [show a]
    show (NOT a)    = "not(" ++ show a ++ ")" --format "not({0})"       [show a]
    show (DELAY a)  = "delay(" ++ show a ++ ")" --format "delay({0})"     [show a]
    
instance Show Command where
    show (Input a)        = "input(" ++ show a ++ ")" --format "input({0})"  [show a]
    show (Output a)       = "output(" ++ show a ++ ")" --format "output({0})" [show a]
    show (Definition a b) = show a ++ " = " ++ show b --format "{0} = {1}"   [show a, show b]
    
instance Show TextLine where
    show (Comment a) = "# " ++ a
    show (Code a (Comment "")) = show a
    show (Code a Empty) = show a
    show (Code a c) = (show a) ++ " " ++ (show c)
    show Empty = ""
    
instance Show Program where
    show (Program a) = join $ map show a where
		join xs = foldl1 joiner xs
		joiner a b = a ++ "\n" ++ b

instance Enum Identifier where
  toEnum = Identifier . show
  fromEnum = read . str
  --succ = toEnum . (+1) . fromEnum
  --pred = toEnum . (-1) . fromEnum

    
validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_]["