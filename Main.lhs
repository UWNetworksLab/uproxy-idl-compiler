%  USE XETEX TO MAKE THIS DOCUMENT

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
%\long\def\ignore#1{}
\maketitle
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
import Parser
import Analyzer
import Generator

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.IO
import System.Exit
import Data.Either.Unwrap
\end{code}

\section{Driver}
The application driver parses arguments and runs the pipeline.


\subsection{Options and Parsing}
The configuration for the driver is via
\link{http://www.haskell.org/haskellwiki/High-level\_option\_handling\_with\_GetOpt}{\texttt{GetOpt}}.
The primary options are:
\begin{enumerate}
\item \texttt{optLanguage} --- generated output language.  Defaults to Typescript.
\item \texttt{optIPC} --- IPC mechanism.  Defaults to freedom.
\item \texttt{optInput} --- input, by default \texttt{stdin}.
\item \texttt{optPrefix} --- prefix for generated filenames.  Defaults to \texttt{input}.
\item \texttt{optOutputDir} --- output directory.  Defaults to current directory.
\item \texttt{optMakeOutputDir} --- whether to create the output
  directory, or just fail if it doesn't exist. Defaults to false.
\end{enumerate}

\begin{code}
data Options = Options { optLanguage :: Generator.Language
                       , optIPC :: Generator.IPCMechanism  
                       , optInput :: IO String
                       , optPrefix :: String
                       , optOutputDir :: IO FilePath
                       , optMakeOutputDir :: Bool 
                       }

startOptions = Options { optLanguage = Generator.Typescript
                       , optIPC = Generator.Freedom
                       , optInput = getContents
                       , optPrefix = "input"
                       , optOutputDir = getCurrentDirectory
                       , optMakeOutputDir = False }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        -- as a convenience, pull the filename and make it the prefix.
        (ReqArg
            (\arg opt -> do let prefix = takeBaseName arg
                            return $ opt { optInput = readFile arg, optPrefix = prefix })
            "FILE")
        "Input file"
 
    , Option "d" ["outputdir"]
        (ReqArg
            (\arg opt -> return opt { optOutputDir = (return arg) })
            "DIR")
        "Output directory"
 
    , Option "l" ["language"]
        (ReqArg
            (\arg opt -> do let lang = parseLang arg
                            if isLeft lang
                              then return opt { optLanguage = (fromRight lang) }
                              else do hPutStrLn stderr $ fromLeft lang
                                      exitWith $ ExitFailure 1)
            "LANG")
        "Enable verbose messages"
      
    , Option "c" ["ipc"]
        (ReqArg
            (\arg opt -> do let ipc = parseIPC arg
                            if isLeft ipc
                               then return opt { optIPC = (fromRight ipc) }
                               else do hPutStrLn stderr $ fromLeft ipc
                                       exitWith $ ExitFailure 1)
            "IPC")
        "IPC Mechanism to Use in Generated Code"
    , Option "p" ["prefix"]
        (ReqArg
            (\arg opt -> return opt { optPrefix = arg }) 
            "PREFIX")
        "Prefix for generated files."
 
    , Option "V" ["version"]
        (NoArg
            (\_ -> do hPutStrLn stderr "Version 0.01"
                      exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do prg <- getProgName
                      hPutStrLn stderr (usageInfo prg options)
                      exitWith ExitSuccess))
        "Show help" ]
\end{code}

\subsection{Driver}

First we parse options.
\begin{code}
main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
\end{code}

Next we parse the input, make the output directory, analyze it, and send it over to the generator.
\begin{code}
    programText <- optInput opts
    decls <- parseDeclarations (optPrefix opts) programText
    let analysis = analyzeDeclarations decls
    if isLeft analysis
      then do mapM_ (\n -> putStrLn ("ERROR: " ++ n)) $ fromLeft analysis 
              putStrLn "Exiting from errors."
      else do let (warnings, analyzedDecls) = fromRight analysis
              mapM_ (\n -> putStrLn ("WARNING: " ++ n)) warnings
              dir <- optOutputDir opts
              unit <- if (optMakeOutputDir opts) 
                        then do createDirectoryIfMissing True dir
                        else return ()
              generateText (optLanguage opts) (optIPC opts) dir decls
\end{code}

\end{document}

