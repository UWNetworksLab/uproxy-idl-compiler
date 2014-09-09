% Local Variables:
% mode: latex 
% mmm-classes: literate-haskell-latex
% End:

\lstset{firstnumber=1}
\section{Analysis}

The analyzer validates the input before we attempt to generate code.
It's also a good place for warnings about style or API misuse.

\begin{code}
module Analyzer (analyzeDeclarations) where
-- Change to Control.Monad.Except for later versions of mtl.
import Control.Monad.Error
import Data.Either
import Data.List (intercalate)
import Data.Maybe
import Data.Tuple

import Options
import Parser
\end{code}

\subsection{Warnings}

Right now, we just leave errors as strings.  Warnings, however have an
\textit{Enum} type that maps each type of warning to an integer.  The
integer lets users identify them uniquely (even if we customize the
warning message to the specific instance of the indicated problem),
and could be used later for command-line options (or pragmas) to
control them individually.

\begin{code}
data NamedWarning = WarnNoExportedDeclarations
                  | WarnNoPromises
                  | UnnamedWarning
                    deriving (Eq, Show)
warnTable = [(WarnNoExportedDeclarations, 100), (WarnNoPromises, 101),
             (UnnamedWarning, -1)]

instance Enum NamedWarning where
    fromEnum = fromJust . flip lookup warnTable
    toEnum = fromJust . flip lookup (map swap warnTable)

data Warning = WarnLocation { wLine :: Int, wCol :: Int, wWarnName :: NamedWarning,
                              wMessage :: String }
             | WarnGeneral { wgWarnName :: NamedWarning, wgMessage :: String }
             deriving (Eq)
                      
instance Show Warning where
  show loc@(WarnLocation {}) =
    (show $ wLine loc) ++ ":" ++ (show $ wCol loc) ++ " (" ++
    (show $ fromEnum $ wWarnName loc) ++ "):: " ++ (wMessage loc)
  show gen@(WarnGeneral {}) =
    "(" ++ (show $ fromEnum $ wgWarnName gen) ++ "):: " ++ (wgMessage gen)

\end{code}

\subsection{Validation Checks}
An immediately undesirable result is to call the \x\ tool and get no
output.  So, we check for two cases where this happens: when the input
doesn't export any interfaces, or when exported interfaces don't have
any IPC-enabled methods (marked with \textit{Promise<>} return types).
\begin{code}
  
type SuccessfulInput = ([Warning], [Parser.Class])
type EndResult = Either String SuccessfulInput

interfaceHasPromises :: Class -> Bool
interfaceHasPromises cls =
  let methodHasPromises :: Method -> Bool
      methodHasPromises meth =
        let retTypeIsPromise ty = (typeName ty) == "Promise" &&
                                  (length . typeArgs) ty > 0
        in maybe False retTypeIsPromise $ methodReturn meth
  in any methodHasPromises $ classMethods cls

-- |Verifies that exported interfaces have some async (e.g.,
-- Promise<>) members.
exportedInterfaceHasPromises :: Options -> SuccessfulInput -> EndResult
exportedInterfaceHasPromises opts input@(warnings, decls) =
    let exported = filter classExported decls
        noPromises = WarnGeneral {
          wgWarnName = WarnNoPromises,
          wgMessage = "No Promise<> return types on an exported interface: "
                      ++ (intercalate ", " $ map className $ 
                          filter (not . interfaceHasPromises) exported) }
    in if any interfaceHasPromises exported
       then Right input
       else Right (warnings ++ [noPromises], decls)

-- |Verifies that at least one declaration has 
declarationsExport :: Options -> SuccessfulInput -> EndResult
declarationsExport opts input@(warnings, decls) =
  let anyExported = any classExported decls
      noneFound = WarnGeneral {
        wgWarnName = WarnNoExportedDeclarations,
        wgMessage = "No exported interfaces found.  No output will be generated." }
  in if anyExported
     then Right input
     else Right (warnings ++ [noneFound], decls)
\end{code}

\subsection{Driver}

The driver function determines which validation checks to try, and then 
runs them.  It uses the \textit{Either} monad defined in 
\textit{Control.Monad.Error}.  The first function that returns 
\textit{Left} will end the analysis and return that alone.  The string 
attached to the Left will be an error message.

\begin{code}

-- |Put any validation rules here.  Returns Right only if it's
-- acceptable.  Left indicates fatal errors.  The array of strings on
-- the right indicate non-fatal warnings.
analyzeDeclarations :: Options -> [Parser.Class] -> EndResult
analyzeDeclarations opts decls =
    -- 'operators' is currently a static list, but could be determined
    -- from additional arguments to analyzeDeclarations.
    let operators = [declarationsExport opts, exportedInterfaceHasPromises opts]
    in foldl (>>=) (Right ([], decls)) operators
\end{code}
