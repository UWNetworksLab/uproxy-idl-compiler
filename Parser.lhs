module Parser ( parseDeclarations
              , module Language.TypeScript.Types) where
import Language.TypeScript
import Language.TypeScript.Types
import Text.ParserCombinators.Parsec.Prim (parse)
import Data.Either.Unwrap

-- |Parse input text and return the parsed form, or output parse
-- errors and return an empty list.
parseDeclarations :: String -> String -> IO [DeclarationElement]
parseDeclarations filename text =
  do let parseResult = parse declarationSourceFile filename text
     if isRight parseResult
        then return $ fromRight parseResult
        else do putStrLn $ show $ fromRight parseResult
                return []
