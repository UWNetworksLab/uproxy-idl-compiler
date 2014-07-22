module Generator (parseLang, parseIPC, generateText, module Generate.Types) where
import Data.Char
import System.IO

import Parser
import Generate.Types 
import Generate.Typescript

parseLang :: String -> Either String Language
parseLang s = case (map toLower s) of
                "typescript" -> Right Typescript
                "ts" -> Right Typescript
                otherwise -> Left $ "Unrecognized language " ++ s

parseIPC :: String -> Either String IPCMechanism
parseIPC s = case (map toLower s) of
                "freedom" -> Right Freedom
                otherwise -> Left $ "Unrecognized IPC Mechanism " ++ s

supportedConfiguration :: Language -> IPCMechanism -> Bool
supportedConfiguration Typescript Freedom = True

generateText :: Language -> IPCMechanism -> FilePath -> [Parser.DeclarationElement] -> IO ()
generateText lang ipc source decls = do
  if supportedConfiguration lang ipc
     then case lang of
            Typescript -> do generateTS ipc source decls
     else do hPutStrLn stderr $ ("Cannot generate code in " ++ (show lang) ++
                                 " for IPC mechanism " ++ (show ipc))
