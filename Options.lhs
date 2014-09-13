\lstset{firstnumber=1}
\subsection{Options}\label{Options}
The configuration for the driver is via
\link{http://www.haskell.org/haskellwiki/High-level\_option\_handling\_with\_GetOpt}{\texttt{GetOpt}}.
The primary options are:
\begin{enumerate}
\item \texttt{optLanguage} --- generated output language.  Defaults to \emph{Typescript}.
\item \texttt{optIPC} --- IPC mechanism.  Defaults to \emph{FreedomMessaging}.
\item \texttt{optInput} --- input, by default \texttt{stdin}.
\item \texttt{optPrefix} --- prefix for generated filenames.  Defaults to \texttt{input}.
\item \texttt{optOutputDir} --- output directory.  Defaults to current directory.
\item \texttt{optMakeOutputDir} --- whether to create the output
  directory, or just fail if it doesn't exist.  Defaults to false.
\item \texttt{optRestArgCount} --- when passing \emph{...rest} to an
  API, how many arguments to present to IPC mechanisms that can't
  handle variable numbers of arguments.  Defaults to 10.
\item \texttt{optVerbose} --- verbosity of output.  Default is 1, with
  higher numbers being more verbose.  0 is the 'STFU' option.
\item \texttt{optMerge} --- merge changes in existing artifacts.
  Defaults to False.  Only useful for \texttt{.json} files.
\end{enumerate}

\begin{code}
module Options (Options(..), getOptions) where
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.IO
import System.Exit
import Data.Either.Unwrap

import Generate.Types as T

data Options = Options { optLanguage :: T.Language
                       , optIPC :: T.IPCMechanism
                       , optInput :: IO String
                       , optPrefix :: String
                       , optOutputDir :: IO FilePath
                       , optMakeOutputDir :: Bool
                       , optRestArgCount :: Int
                       , optVerbose :: Int
                       , optMerge :: Bool
                       }

startOptions = Options { optLanguage = T.Typescript
                       , optIPC = T.FreedomMessaging
                       , optInput = return ""
                       , optPrefix = "input"
                       , optOutputDir = getCurrentDirectory
                       , optMakeOutputDir = False
                       , optRestArgCount = 10
                       , optVerbose = 1
                       , optMerge = False
                       }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        -- as a convenience, pull the filename and make it the prefix.
        (ReqArg
            (\arg opt -> do let prefix = takeBaseName arg
                            return $ opt { optInput = readFile arg, optPrefix = prefix })
            "FILE")
        "Input file, also setting the prefix to its directory."

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
                            if isRight ipc
                               then return opt { optIPC = (fromRight ipc) }
                               else do hPutStrLn stderr $ fromLeft ipc
                                       exitWith $ ExitFailure 1)
            "IPC")
        "IPC Mechanism to Use in Generated Code"

    , Option "m" ["merge"]
        (NoArg
            (\opt -> return opt { optMerge = True }))
        "Merge changes into existing artifacts"

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

    , Option "v" ["verbose"]
        (OptArg (\n opt ->
                  case n of
                    Just s -> return opt { optVerbose = (read s) :: Int }
                    Nothing -> return opt { optVerbose = 2 })
         "Verbosity")
        "Output verbosity"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do prg <- getProgName
                      hPutStrLn stderr (usageInfo prg options)
                      exitWith ExitSuccess))
        "Show help"

    , Option "r" ["restargs"]
        (ReqArg
            (\arg opt -> return opt { optRestArgCount = read arg })
            "NUMARGS")
      "Number of arguments to expand ...rest args in signatures." ]

-- |Given the arguments to the command line, returns a filled Options and the
-- remaining arguments.
getOptions :: [String] -> IO (Options, [String])
getOptions args = do
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  if length errors > 0
  then do mapM_ (hPutStrLn stderr) errors
          exitWith $ ExitFailure 1
  else do opts <- foldl (>>=) (return startOptions) actions
          return (opts, nonOptions)
\end{code}
