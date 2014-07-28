\section{Typescript Generation}

\begin{code}
module Generate.Typescript where

import Data.Char
import Data.Maybe
import Data.List
import Language.TypeScript.Pretty
import Text.StringTemplate
import System.FilePath.Posix

import Generate.Types
import Parser
\end{code}

\subsection{Indentation}
\begin{code}
indentString = "  "

-- |Simple text indenter.
indentText :: Int -> String -> String
indentText depth input =
  let expanded = concat $ take depth $ repeat indentString
      replaceChar c | c == '\n' = "\n" ++ expanded
                    | otherwise = [c]
  in expanded ++ (concatMap replaceChar input)

-- |Convert camelCase to hyphen-separated (fooBar -> foo-bar)
hyphenSeparateHumps str =
  let convertChar c = if isAlpha c && isUpper c
                      then ['-', toLower c]
                      else [c]
  in concat $ map convertChar str

\end{code}

\subsection{Type Printing}
\begin{code} 
-- Printers for different parts of the input grammar.  By "Print", we
-- mean return a printable string value.

-- |Print the type specified
typeName :: Type -> String
typeName (Predefined (AnyType)) = "any"
typeName (Predefined (NumberType)) = "number"
typeName (Predefined (BooleanType)) = "boolean"
typeName (Predefined (StringType)) = "string"
typeName (Predefined (VoidType)) = "void"
typeName (TypeReference (TypeRef (TypeName _ nm) _)) = nm
typeName (ObjectType ty) = undefined -- not allowed!
typeName (ArrayType ty) = "[" ++ typeName ty ++ "]"
typeName (FunctionType typep parms ty) =
  let prefix = typeName ty
  in prefix ++ " function(" ++ (intercalate ", " $ map paramName parms) ++ ")"
typeName (ConstructorType typep parms ty) = 
  let prefix = typeName ty
  in prefix ++ " constructor(" ++ (intercalate ", " $ map paramName parms) ++ ")"

-- |Print the name of a parameter
paramName :: Parameter -> String
paramName (RequiredOrOptionalParameter _ nm _ _) = nm
paramName (RestParameter nm _) = nm

-- |Print the return type, or  
returnType :: Maybe Type -> String
returnType ret = if isJust ret then typeName (fromJust ret) else ""

\end{code}

\subsection{IPC--Specific Code Generation}
\begin{code} 
-- |Generate the IPC body of the generated body -- an IPC call 
generateInterfaceIPCCall :: IPCMechanism -> Int -> String -> [Parameter] -> String
generateInterfaceIPCCall ipc depth method params =
    case ipc of
      Freedom -> let message = "\"" ++ hyphenSeparateHumps method ++ "\""
                     callBody = intercalate ", " $ message :  map paramName params
                 in "freedom.send(" ++ callBody ++ ");"
      -- only IPC mechanism supported.  Add others here.

-- |Generate typescript for an interface's body
generateInterfaceBody :: IPCMechanism -> Int -> TypeBody -> String
generateInterfaceBody ipc depth declBody =
  let generateInterfaceBody' (_, (MethodSignature nm opt (ParameterListAndReturnType _ params ret))) =
        let templateText = unlines [
                            "$signature$ {",
                            indentString ++ "$invocation$",
                            "}" ]
            args = intercalate ", " $ map show params
            signatureReturn = returnType ret
            template = newSTMP templateText :: StringTemplate String
        in render $ setManyAttrib [("signature", signatureReturn ++ " " ++ nm ++ "(" ++
                                                 (concat $ intersperse ", " $ map paramName params) ++ ")"),
                                   ("invocation", generateInterfaceIPCCall ipc depth nm params)] template
      generateInterfaceBody' (_, _) = ""  -- only produce code for method signatures.
      TypeBody decls = declBody
  in indentText depth $ intercalate "\n" $ map generateInterfaceBody' decls

-- |Generate typescript for an interface declaration.
generateInterface :: IPCMechanism -> Int -> DeclarationElement -> String
generateInterface ipc depth decl@(InterfaceDeclaration _ exported
                                  iface@(Interface _ name _ _ body)) =
  if isJust exported
     then let templateText = unlines [
                              "interface $stubname$ {",
                              "$body$",
                              "}"
                             ]
              bodyText = generateInterfaceBody ipc (depth+1) body 
              template = newSTMP templateText :: StringTemplate String
              filledTemplate = setManyAttrib [("stubname", name),
                                              ("body", indentText depth bodyText)] template
          in render filledTemplate
     else ""  -- ignore unexported interfaces.
\end{code}
\begin{code} 

-- |Primary driver for generating typescript code.
generateTS :: IPCMechanism -> FilePath -> [DeclarationElement] -> IO ()
generateTS ipc sourceDir decls = do
  let generatedFilename = (takeDirectory sourceDir) </> ((takeBaseName source) ++ "_stub.ts")
  putStrLn $ "> Outputting to file " ++ generatedFilename
  let stubText = concat $ map (generateInterface ipc 1) decls
  writeFile generatedFilename stubText

\end{code}

