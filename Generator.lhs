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
module Generator (generateText, module Generate.Types) where
import Data.Char
import Data.List (intercalate)
import System.IO

import Options
import Parser
import Generate.Types
import Generate.Typescript
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

generateText :: Options -> FilePath -> [Class] -> IO ()
generateText options source decls = do
  let lang = optLanguage options
      ipc = optIPC options
  if supportedConfiguration lang ipc
     then case lang of
            Typescript -> do generateTS options source decls
     else do hPutStrLn stderr $ ("Cannot generate code in " ++ (show lang) ++
                                 " for IPC mechanism " ++ (show ipc))
\end{code}

\input{Generate/Types.lhs}
\input{Generate/Typescript.lhs}
