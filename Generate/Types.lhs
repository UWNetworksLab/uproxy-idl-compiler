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
module Generate.Types where

data Language = Typescript deriving (Eq, Show)
data IPCMechanism = FreedomMessaging | FreedomJSON deriving (Eq, Show)
\end{code}

