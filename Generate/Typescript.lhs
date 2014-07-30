% Local Variables:
% mode: latex 
% mmm-classes: literate-haskell-latex
% End:

\lstset{firstnumber=1}
\section{Typescript Generation}
We generate Typescript code with either \ident{FreedomMessaging} or
\ident{FreedomJSON} IPC types.  The generated skeleton file is
identical in both cases.  The generated stubs are quite different.  In
the \ident{FreedomMessaging} case, it's an implementation of the
interface in Typescript, with method bodies that generate the
\ident{emit(``message'')} calls.

In \ident{FreedomJSON}, the stub is part of a generated JSON object
that's interpreted by the Freedom runtime.
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Generate.Typescript where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson -- (FromJSON(..), ToJSON(..), Object, (.:), decode, object)
import qualified Data.Aeson.TH as TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile, pack, unpack)
import Data.Char
import Data.Data (Typeable(..), Data(..))
import Data.HashMap.Strict (toList)
import Data.List
import Data.Maybe
import Data.Text (Text(..), pack, unpack)
import Data.Vector (fromList)
import Language.TypeScript.Pretty
import Text.StringTemplate
import System.FilePath.Posix
import System.IO (stderr, hPutStrLn)

import Generate.Types
import Parser
\end{code}

\subsection{Indentation and Identifier Generation}
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
% $ <- to keep literate-haskell-mmm happy.

\subsection{Type Printing}
The parsed representation of the input is quite verbose, so we have to
spend a good amount of code to pull the desired properties from the
rest.

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
paramName (RestParameter nm _) = "..." ++ nm

-- |Print the return type, or  
returnType :: Maybe Type -> String
returnType ret = if isJust ret then typeName (fromJust ret) else ""

-- |Print the name of an interface name, if it's exported.  Otherwise Nothing.
exportedInterfaceName :: DeclarationElement -> Maybe String
exportedInterfaceName (InterfaceDeclaration _ (Just _) (Interface _ nm _ _ _)) =
    Just nm
exportedInterfaceName _ = Nothing
  

\end{code}
% $

\subsection{Typescript Code Generation}
For \ident{FreedomMessaging}, we generate Typescript twice: first for
the stub (which calls \ident{emit()}) and second for the skeleton.
The latter time, we simply have an empty method body.
\ident{generateInterfaceBody} takes a \ident{Maybe IPCMechanism} to
indicate whether there is an IPC mechanism to generate text for (in
the stub case), or \ident{Nothing} to generate, for the skeletal
case.


\begin{code}
-- |Generate the IPC call of the generated method body.
generateInterfaceIPCCall :: IPCMechanism -> Int -> String -> [Parameter] -> String
generateInterfaceIPCCall ipc depth method params =
    case ipc of
      FreedomMessaging ->
        let message = "\"" ++ hyphenSeparateHumps method ++ "\""
            callBody = intercalate ", " $ message : map paramName params
        in "freedom.send(" ++ callBody ++ ");"
      -- only IPC mechanism supported.  Add others here.

-- |Generate typescript for an interface's body. Pass 'Nothing' for
-- the IPC type if the body should be an empty skeleton.
generateInterfaceBody :: Maybe IPCMechanism -> Int -> TypeBody -> String
generateInterfaceBody ipc depth declBody =
  let generateInterfaceBody' (_, (MethodSignature nm opt (
                                     ParameterListAndReturnType _ params ret))) =
        let templateText = unlines [
                            "$signature$ {",
                            indentString ++ "$invocation$",
                            "}" ]
            args = intercalate ", " $ map show params
            signatureReturn = returnType ret
            template = newSTMP templateText :: StringTemplate String
        in render $ setManyAttrib [
          ("signature", signatureReturn ++ " " ++ nm ++ "(" ++
                        (concat $ intersperse ", " $ map paramName params) ++ ")"),
          ("invocation", if isJust ipc 
                           then generateInterfaceIPCCall (fromJust ipc) depth nm params
                           else "// write your implementation of " ++ nm ++ " here")] template
      generateInterfaceBody' (_, _) = ""  -- only produce code for method signatures.
      TypeBody decls = declBody
  in indentText depth $ intercalate "\n" $ map generateInterfaceBody' decls

-- |Generate typescript for an interface declaration.
generateInterface :: Maybe IPCMechanism -> Int -> DeclarationElement -> String
generateInterface ipc depth decl@(InterfaceDeclaration _ exported
                                  iface@(Interface _ name _ _ body)) =
  if isJust exported
     then let templateText = unlines [ "interface $stubname$ {",
                                       "$body$",
                                       "}" ]
              bodyText = generateInterfaceBody ipc (depth+1) body 
              template = newSTMP templateText :: StringTemplate String
              filledTemplate = setManyAttrib [
                ("stubname", name),
                ("body", indentText depth bodyText)] template
          in render filledTemplate
     else ""  -- ignore unexported interfaces.
\end{code}
%$

\subsection{JSON Generation}
As we're using the Typescript generator for our skeleton
implementation, we only have the JSON to generate separately for
\ident{FreedomJSON}.  

\paragraph{\ident{Text} vs \ident{String}}
The parser library uses \ident{String}, which is perfectly reasonable.
But \pkgid{aeson} uses \ident{Text}, a better general-purpose string
type.  So we have to slowly convert the input strings to \ident{Text}.
\ident{pack} does this, but we have to find good places for putting in
the conversions.  Generally, we will try to avoid the \ident{pack}
until the last moment, except when it would require an extra list
traversal.

\paragraph{Freedom Manifest}
We use the \pkgid{aeson} library in Haskell for this, which can
directly encode/decode Haskell types using the \ident{Typeable}
auto-derived typeclass.  With the ability to decode an existing JSON
file, we can incrementally update one instead of wiping it each time.

We can mostly use the \pkgid{aeson} default (de)serialization, but
have to customize it for \ident{FreedomAPI} and
\ident{FreedomAPIEntry}, as they have unsupported schemas.

\begin{code}
data FreedomApp = App { script :: Text } deriving (Show, Eq)
data FreedomConstraints = Constraints { isolation :: Text } deriving (Show, Eq)
data FreedomAPIEntry = ApiEntry 
    { apiType :: Text
    , apiValue :: [Text]
    , apiRet :: [Text] 
    } deriving (Show, Eq)

data FreedomAPI = Api { entries :: [(Text, [FreedomAPIEntry])] } deriving (Show, Eq)
data FreedomManifest = Manifest 
    { name :: Text
    , description :: Text
    , app :: FreedomApp
    , constraints :: FreedomConstraints
    , provides :: [Text]
    , api :: FreedomAPI
    , dependencies :: [Text]
    , permissions :: [Text]
    } deriving (Show, Eq)

$(TH.deriveJSON TH.defaultOptions ''FreedomApp)
$(TH.deriveJSON TH.defaultOptions ''FreedomConstraints)
$(TH.deriveJSON TH.defaultOptions {TH.fieldLabelModifier = drop 4} ''FreedomAPIEntry)
$(TH.deriveJSON TH.defaultOptions ''FreedomManifest)

-- |FreedomAPI uses custom keys, so do this by hand.
instance FromJSON FreedomAPI where
    parseJSON (Object v) = 
        let 
            readEntry (k, obj) = 
                let parsed = fromJSON obj
                    interpret (Error _) = []
                    interpret (Success s) = s
                in (k, interpret parsed) :: (Text, [FreedomAPIEntry])
            allEntries = map readEntry $ toList v
        in mzero -- Api <$> (toList v <$> ZipList readEntry) -- FIXME
    parseJSON _ = mzero

instance ToJSON FreedomAPI where
    toJSON api = object $ map (\(k,v) -> (k, toJSON v)) $ entries api

\end{code}
%$
\paragraph{Method Generation}
The code to generate each method is pretty simple: fill in a
\ident{FreedomAPI} for each exported interface, and within each, a
\ident{FreedomAPIEntry} for each declared method.

Sadly we have to pattern match two-deep here, due to the verbose AST
generated from the parser.
\begin{code}
generateJSONMethod :: String -> ParameterListAndReturnType -> FreedomAPIEntry
generateJSONMethod nm (ParameterListAndReturnType _ params ret) =
    let paramNames = map paramName params
        retType = if isJust ret then [typeName (fromJust ret)] else []
    in ApiEntry { apiType = "method", apiValue = map pack paramNames, apiRet = map pack retType }

generateJSONApis :: DeclarationElement -> (Text, [FreedomAPIEntry])
generateJSONApis (InterfaceDeclaration _ _ iface@(Interface _ name _ _ body)) =
    let (TypeBody bodymems) = body
        makeEntry (_,(MethodSignature nm _ params)) = Just $ generateJSONMethod nm params
        makeEntry _ = Nothing
    in (pack name, mapMaybe makeEntry bodymems)

generateJSONApi :: [DeclarationElement] -> FreedomAPI
generateJSONApi decls =
    let processDecl decl = 
            let iface = exportedInterfaceName decl
            in if isJust iface
               then Just $ generateJSONApis decl
               else Nothing
        apis = mapMaybe processDecl decls
    in Api { entries = apis }
\end{code}
% $

\paragraph{JSON Driver}
The driver starts by building a default instance of \ident{FreedomManifest} 
and then fills in the methods.  The default instance can be read from the file
if it exists, or a simple empty one derived from the declarations.

\begin{code}
-- |A unique generation function that can merge a prior definition into the current one.
generateJson :: FilePath -> Bool -> [DeclarationElement] -> IO ()
generateJson path merge decls = do
  let firstIface = head $ mapMaybe exportedInterfaceName decls
      moduleName = hyphenSeparateHumps firstIface
      dflSkeleton = Manifest 
                    { name = pack moduleName
                    , description = pack ""
                    , app = App { script = pack $ moduleName ++ ".js" }
                    , constraints = Constraints { isolation = pack "never" }
                    , provides = [pack firstIface]
                    , api = Api { entries = [] }
                    , dependencies = []
                    , permissions = []
                    }
  skel <- if merge
          then do contents <- readFile path
                  hPutStrLn stderr $ "Reading manifest from " ++ path
                  let parsed = decode $ C8.pack contents
                  if isJust parsed
                  then return $ fromJust parsed
                  else do fail $ path ++ ": failed to parse.  " ++ 
                                        "Aborting instead of clobbering."
          else return dflSkeleton
  let result = skel { api = generateJSONApi decls }
  BL.writeFile path $ encodePretty result 
\end{code}

\subsection{Driver}
\begin{code}
-- |Primary driver for generating typescript code.
generateTS :: IPCMechanism -> FilePath -> [DeclarationElement] -> IO ()
generateTS ipc sourceDir decls = do
  let exportedInterfaces = mapMaybe exportedInterfaceName decls
  if length exportedInterfaces > 0
  then do let generatedFilenameBase = sourceDir </> (head exportedInterfaces)
          putStrLn $ "> Parsed input AST: " ++ (intercalate "\n    " $ map show decls)
          putStrLn $ "\n\n> Parsed input: " ++ renderDeclarationSourceFile decls
          case ipc of
            FreedomMessaging ->  do 
                     putStrLn $ "\n\n> Outputting to files " ++ generatedFilenameBase ++ "_(stub|skel).ts"
                     let stubText = concatMap (generateInterface (Just ipc) 1) decls
                         skelText = concatMap (generateInterface Nothing 1) decls
                     writeFile (generatedFilenameBase ++ "_stub.ts") stubText
                     writeFile (generatedFilenameBase ++ "_skel.ts") skelText
            FreedomJSON -> do
                     putStrLn $ "\n\n> Output to files " ++ generatedFilenameBase ++ "_stub.ts and " ++
                               generatedFilenameBase ++ ".json."
                     let skelText = concatMap (generateInterface Nothing 1) decls
                     generateJson (generatedFilenameBase ++ ".json") True decls
                     writeFile (generatedFilenameBase ++ "_skel.ts") skelText
  else hPutStrLn stderr "> Failure.  No exported interfaces."
\end{code}


