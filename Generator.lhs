% Local Variables:
% mode: latex 
% mmm-classes: literate-haskell-latex
% End:

\lstset{firstnumber=1}
\section{Code Generation}
The code generation system is large enough to warrant a few sections.
This section is dedicated to the support systems around code
generation: configuration validation and option parsing.

\begin{code}
module Generator (parseLang, parseIPC, generateText, module Generate.Types) where
import Data.Char
import Data.List (intercalate)
import System.IO

import Parser
import Generate.Types 
import Generate.Typescript
\end{code}

\subsection{Option Parsing}
We parse both languages and IPC implementations.  They use the same
association-list matcher, \textit{parseImpl}, listed below.
\begin{code}
-- |Look up a string in an association list, or return an error
-- message that lists possible options.
parseImpl :: [(String, a)] -> String -> String -> Either String a
parseImpl elements typeName nm =
    let found = filter (\(n,v) -> n == (map toLower nm)) elements
    in if length found > 0
       then Right $ snd $ head found
       else Left $ "'" ++ nm ++ "' is not a valid " ++ typeName ++ ".  Those are: " ++ (
                   intercalate ", " $ map fst elements)

languages = [("typescript", Typescript), ("ts", Typescript)]
ipcs = [("messaging", FreedomMessaging), ("json", FreedomJSON)]

parseLang s = parseImpl languages "language" s
parseIPC s = parseImpl ipcs "IPC mechanism" s
\end{code}

\subsection{Configuration Validation}
We validate and run a specified configuration by checking if we
support the configuration, and if so, dispatch it to the appropriate
generation subsystem.
\begin{code}
supportedConfiguration :: Language -> IPCMechanism -> Bool
supportedConfiguration Typescript FreedomMessaging = True
supportedConfiguration Typescript FreedomJSON = True
supportedConfiguration _ _ = False

generateText :: Language -> IPCMechanism -> FilePath -> [Parser.DeclarationElement] -> IO ()
generateText lang ipc source decls = do
  if supportedConfiguration lang ipc
     then case lang of
            Typescript -> do generateTS ipc source decls
     else do hPutStrLn stderr $ ("Cannot generate code in " ++ (show lang) ++
                                 " for IPC mechanism " ++ (show ipc))
\end{code}

\input{Generate/Types.lhs}
\input{Generate/Typescript.lhs}
