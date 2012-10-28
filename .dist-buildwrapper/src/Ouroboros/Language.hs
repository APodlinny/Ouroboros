module Ouroboros.Language (
    Program(..),
    Line(..),
    Command(..),
    Expression(..)
) where

import Text.Format

data Program = Program [Line]
data Line = Comment String | Code Command
data Command = Input String |
               Output String |
               Definition String Expression
               
data Expression = AND String String |
                  NAND String String |
                  OR String String |
                  NOR String String |
                  XOR String String |
                  BUF String |
                  NOT String |
                  DELAY String
                  
instance Show Expression where
    show (AND a b) = 