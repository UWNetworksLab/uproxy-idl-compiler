\begin{code}
module Analyzer (analyzeDeclarations) where
import Parser
import Control.Monad.Except
import Data.Either
import Data.Maybe
import Data.Tuple

data NamedWarning = WarnNoExportedDeclarations
                  | UnnamedWarning
                    deriving (Eq, Show)
warnTable = [(WarnNoExportedDeclarations, 100), (UnnamedWarning, -1)]

instance Enum NamedWarning where
    fromEnum = fromJust . flip lookup warnTable
    toEnum = fromJust . flip lookup (map swap warnTable)

data Warning = WarnLocation { wLine :: Int, wCol :: Int, wWarnName :: NamedWarning,
                              wMessage :: String }
             | WarnGeneral { wgWarnName :: NamedWarning, wgMessage :: String }
             deriving (Eq) 
instance Show Warning where
  show loc@(WarnLocation {}) = (show $ wLine loc) ++ ":" ++ (show $ wCol loc) ++ " (" ++
                               (show $ fromEnum $ wWarnName loc) ++ "):: " ++ (wMessage loc)
  show gen@(WarnGeneral {}) = "(" ++ (show $ fromEnum $ wgWarnName gen) ++ "):: " ++ (wgMessage gen)
  
type SuccessfulInput = ([Warning], [Parser.DeclarationElement])
type EndResult = Either String SuccessfulInput
-- type AnalysisPhase a = a -> m b

-- |Verifies that at least one declaration has 
declarationsExport :: SuccessfulInput -> EndResult
declarationsExport input@(warnings, decls) =
  let exportsInterface (Parser.InterfaceDeclaration _ (Just _) _) = True
      exportsInterface _ = False
      anyExported = any exportsInterface decls
      noneFound = WarnGeneral {
        wgWarnName = WarnNoExportedDeclarations,
        wgMessage = "No exported interfaces found.  No output will be generated." }
  in if not anyExported
     then Right (warnings ++ [noneFound], decls)
     else Right input

    

-- |Put any validation rules here.  Returns Right only if it's
-- acceptable.  Left indicates fatal errors.  The array of strings on
-- the right indicate non-fatal warnings.
analyzeDeclarations :: [Parser.DeclarationElement] -> EndResult
analyzeDeclarations decls =
    -- 'operators' is currently a static list, but could be determined
    -- from additional arguments to analyzeDeclarations.
    let operators = [declarationsExport]
    in foldl (>>=) (Right ([], decls)) operators
\end{code}
