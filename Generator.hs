module Generator where
import Data.Char
import System.IO
import Parser

data Language = Typescript deriving (Eq, Show)

parseLang :: String -> Either String Language
parseLang s = case (map toLower s) of
                "typescript" -> Right Typescript
                "ts" -> Right Typescript
                otherwise -> Left $ "Unrecognized language " ++ s

generateText :: Language -> FilePath -> [Parser.DeclarationElement] -> IO ()
generateText = undefined
