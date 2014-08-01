

\section{Parser}
The parser uses the existing \emph{language-typescript} package and
then canonicalizes input.
\lstset{firstnumber=1}
\begin{code}
module Parser ( parseDeclarations
              , module Language.TypeScript.Types) where
import Language.TypeScript
import Language.TypeScript.Types
import Text.ParserCombinators.Parsec.Prim (parse)
import Data.Either.Unwrap

-- |Simplifies input by canonicalizing different forms (e.g.,
-- interface vs ambient class declarations) into the same underlying
-- types.
canonicalizeDeclarations :: [DeclarationElement] -> [DeclarationElement]
canonicalizeDeclarations decls = undefined

-- |Parse input text and return the parsed form, or output parse
-- errors and return an empty list.
parseDeclarations :: String -> String -> IO [DeclarationElement]
parseDeclarations filename text =
  do let parseResult = parse declarationSourceFile filename text
     if isRight parseResult
        then return $ fromRight parseResult
        else do putStrLn $ show $ fromRight parseResult
                return []
\end{code}
