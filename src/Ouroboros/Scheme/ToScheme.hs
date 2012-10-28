module Ouroboros.Scheme.ToScheme (
    programToScheme
) where

import Ouroboros.Scheme.Definition
import qualified Ouroboros.Language as AST

programToScheme :: AST.Program -> Scheme
programToScheme program = if not $ isProgramValid program then
						Scheme schemeName matrix nodes
					else
						error "Program is not valid."
					where
						schemeName = getName program
						getName (AST.Program lines) = getComment $ 
													  head $ 
													  filter AST.isCommentLine lines
                                                      
-- TODO: add program validation
isProgramValid :: AST.Program -> Bool
isProgramValid program = undefined