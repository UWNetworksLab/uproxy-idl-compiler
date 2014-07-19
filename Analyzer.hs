module Analyzer (analyzeDeclarations) where
import Parser
import Data.Either

-- |Put any validation rules here.  Returns Right only if it's
-- acceptable.  Left indicates fatal errors.  The array of strings on
-- the right indicate non-fatal warnings.
analyzeDeclarations :: [Parser.DeclarationElement] ->
                       Either [String] ([String], [Parser.DeclarationElement])
analyzeDeclarations decls = Right ([], decls)
