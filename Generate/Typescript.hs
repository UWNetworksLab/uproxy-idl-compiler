module Generate.Typescript (generateTS) where

import Text.StringTemplate
import System.FilePath.Posix
import Generate.Types
import Parser


generateStub :: IPCMechanism -> FilePath -> [Parser.DeclarationElement] -> IO ()
generateStub Freedom output decls = do
  let templateText = unlines [
        "interface $stubname$ {",
        "  "
        ]
      stubName = ""
      template = newSTMP templateText :: StringTemplate String
      filledTemplate = setManyAttrib [("stubname", stubName)] template
      stubText = render filledTemplate
  return ()
  
generateTS :: IPCMechanism -> FilePath -> [Parser.DeclarationElement] -> IO ()
generateTS ipc source decls = do
  let generatedFilename = (takeDirectory source) </> ((takeBaseName source) ++ "_stub.ts")
  generateStub ipc generatedFilename decls
