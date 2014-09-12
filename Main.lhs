% USE xelatex TO MAKE THIS DOCUMENT

% Local Variables:
% mode: latex 
% mmm-classes: literate-haskell-latex
% End:

\documentclass[10pt]{article}
\usepackage{listings}
\usepackage{hyperref}
\usepackage{lmodern} 
\usepackage{xcolor}
\usepackage{fullpage}
\hypersetup{
    colorlinks,
    linkcolor={red!50!black},
    citecolor={blue!50!black},
    urlcolor={blue!80!black}
}

\title{X1: Prototype IDL Compiler for Freedom}
\lstloadlanguages{Haskell}
% \lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
     \lstset{
       language=Haskell,
       deletekeywords={System, FilePath, IO, Either, String, Bool,
           False, getContents, readFile, hPutStrLn, stderr, exitWith,
           getProgName, putStrLn},
 %      keywordstyle=\bfseries,
 %      basicstyle=\footnotesize,
 %      frame=leftline,
       basewidth={0.5em,0.45em},
       columns=fullflexible,
       showspaces=false,
       commentstyle=\it,
       stringstyle=\mdseries\rmfamily,
       keywordstyle=\bfseries\rmfamily,
       basicstyle=\small\sffamily,
       showstringspaces=false,
       morecomment=[l]\%,
       xleftmargin=2pt,
       stepnumber=1,
       numbers=left,
       numbersep=5pt,
       numberblanklines=false,
       numberstyle=\ttfamily\tiny\color[gray]{0.3},
       firstnumber=last,
       literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
                {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
                {\\\\}{{\char`\\\char`\\}}1
                {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
                {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
                {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
                {>>}{{>>}}2 {>>=}{{>>=}}2
                {|}{{$\mid$}}1               
     }
\begin{document}
\newcommand{\x}{\emph{X1}}
\newcommand{\freedom}{\texttt{freedom}}
\newcommand{\link}[2]{\href{#1}{#2}\footnote{\url{#1}}}
\newcommand{\ident}[1]{\textit{#1}}  % For referencing identifiers in the source
\newcommand{\pkgid}[1]{\textit{#1}}  % For referencing Haskell packages.
%\long\def\ignore#1{}
\maketitle
\tableofcontents
\newpage
\section{Purpose and Scope}

The \x\  compiler generates a skeletal \freedom\  module implementation and an IPC
stub from a Typescript interface definition.  The module implementation and
stub are type safe, substantially reducing errors in module usage.  The
generated stub code wraps the event names, eliminating a class of client errors.  

\section{Design Overview}
\x\ is built out of three primary parts, which assemble in a pipeline:
\begin{enumerate}
\item \emph{Parser} --- A Typescript declaration-file parser, that
  generates a representation of the interface.  

\item \emph{Analyzer} --- Validates the parsed representation against
  any IDL constraints.

\item \emph{Generator} --- Outputs stub and skeleton code from the representation.
\end{enumerate}

Finally, a \emph{Driver} (called \texttt{main})  runs the pipeline.

\begin{code}
module Main (main) where
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import Data.Either.Unwrap (fromLeft, fromRight, isLeft, isRight)

import Options
import Parser
import Analyzer
import Generator
\end{code}

\section{Driver}
The application driver parses arguments and runs the pipeline.

\subsection{\texttt{main}}
First we parse options.  See Section~\ref{Options} for details.  Next,
we look for inputs to process.  They can be specified via options or
as non-options.  Try both.
\begin{code}
main = do
    args <- getArgs
    -- Get program options and remaining args.
    (opts, nonOptions) <- getOptions args
    programText <- optInput opts
    -- build a list of possible input sources
    -- TODO(lally): consider using the -i processing path in options
    -- for each input, threading new Options between each input to mutate
    -- optPrefix.
    if optVerbose opts > 1
      then do putStrLn $ show nonOptions
              -- putStrLn $ show programText
              putStrLn "Running over input sources."
              return ()
      else return ()
    let nonOptionSources = map (\f -> do readFile f) nonOptions
        inputSources = if length programText > 0
                       then (return programText) : nonOptionSources
                       else nonOptionSources
    -- and run them all through processInput.
    mapM_ (\s -> do text <- s; processInput text opts) inputSources
\end{code}
% $
Next we parse the input, analyze it, make the output directory,
analyze it, and send it over to the generator.  We do that in a
separate \ident{processInput} function.

\begin{code}
processInput programText opts = do
    (warnings, decls) <- parseDeclarations opts "input" programText
    if optVerbose opts > 1 && length warnings > 0
      then do putStrLn $ unlines $ warnings
              return ()
      else return ()
    let analysis = analyzeDeclarations opts decls
    if optVerbose opts > 1
      then do putStrLn "Finished analysis."
              return ()
      else return ()
    if isLeft analysis
      then do putStrLn ("ERROR " ++ fromLeft analysis)
              putStrLn "Exiting from errors."
      else do let (warnings, analyzedDecls) = fromRight analysis
              mapM_ (\w -> putStrLn ("WARNING " ++ (show w))) warnings
              dir <- optOutputDir opts
              unit <- if (optMakeOutputDir opts)
                        then do createDirectoryIfMissing True dir
                        else return ()
              generateText opts dir decls
\end{code}

\input{Options.lhs}
\input{Parser.lhs}
\input{Analyzer.lhs}
\input{Generator.lhs}
\end{document}

