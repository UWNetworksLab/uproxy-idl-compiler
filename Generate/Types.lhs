% Local Variables:
% mode: latex 
% mmm-classes: literate-haskell-latex
% End:

\lstset{firstnumber=1}
\subsection{Types used in Generation}

\begin{enumerate}
\item \emph{FreedomMessaging} --- \texttt{on}/\texttt{emit} messaging.
\item \emph{FreedomJSON} --- API specified in JSON.
\end{enumerate}
\begin{code}
module Generate.Types (Language(..), IPCMechanism(..), parseLang, parseIPC) where
import Data.Char (toLower)
import Data.List (intercalate)

data Language = Typescript deriving (Eq, Show)
data IPCMechanism = FreedomMessaging | FreedomJSON deriving (Eq, Show)
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

