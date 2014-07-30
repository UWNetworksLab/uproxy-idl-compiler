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
import Parser
import Control.Monad.Except
import Data.Either
import Data.Maybe
import Data.Tuple
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
  
type SuccessfulInput = ([Warning], [Parser.DeclarationElement])
type EndResult = Either String SuccessfulInput

-- |Returns whether the declaration has an 'export' tag.
exportsInterface :: DeclarationElement -> Bool
exportsInterface (Parser.InterfaceDeclaration _ (Just _) _) = True
exportsInterface _ = False

interfaceHasPromises :: DeclarationElement -> Bool
interfaceHasPromises (Parser.InterfaceDeclaration _ _ iface) =
  let (Parser.Interface _ _ _ _ body) = iface
      (TypeBody members) = body
      methodReturn (Parser.MethodSignature nm _ (
                         Parser.ParameterListAndReturnType _ _ (
                            Just (TypeReference retTy)))) =
        Just (nm, retTy)
      methodReturn _ = Nothing
      isPromise (nm, (TypeRef (TypeName _ outer) (Just params))) =
        outer == "Promise" && length params > 1
  in any isPromise $ mapMaybe methodReturn $ map snd members
interfaceHasPromises _ = False

-- |Verifies that exported interfaces have some async (e.g.,
-- Promise<>) members.
exportedInterfaceHasPromises :: SuccessfulInput -> EndResult
exportedInterfaceHasPromises input@(warnings, decls) =
    let exported = filter exportsInterface decls
        noPromises = WarnGeneral {
          wgWarnName = WarnNoPromises,
          wgMessage = "No Promise<> return types on an exported interface."}
    in if any interfaceHasPromises exported
       then Right input
       else Right (warnings ++ [noPromises], decls)

-- |Verifies that at least one declaration has 
declarationsExport :: SuccessfulInput -> EndResult
declarationsExport input@(warnings, decls) =
  let anyExported = any exportsInterface decls
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
\textit{Control.Monad.Except}.  The first function that returns 
\textit{Left} will end the analysis and return that alone.  The string 
attached to the Left will be an error message.

\begin{code}

-- |Put any validation rules here.  Returns Right only if it's
-- acceptable.  Left indicates fatal errors.  The array of strings on
-- the right indicate non-fatal warnings.
analyzeDeclarations :: [Parser.DeclarationElement] -> EndResult
analyzeDeclarations decls =
    -- 'operators' is currently a static list, but could be determined
    -- from additional arguments to analyzeDeclarations.
    let operators = [declarationsExport, exportedInterfaceHasPromises]
    in foldl (>>=) (Right ([], decls)) operators
\end{code}
