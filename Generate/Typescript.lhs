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
import Data.Aeson
import qualified Data.Aeson.TH as TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as C8 (pack)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile, pack, unpack)
import Data.Char
import Data.Data (Typeable(..), Data(..))
import Data.HashMap.Strict (toList)
import Data.List
import Data.Maybe
import Data.Monoid (mappend)
import Data.Ord (comparing)
import qualified Data.Text as T (Text(..), pack, unpack, length)
import Data.Vector (fromList)
import Debug.Trace
import Language.TypeScript.Pretty
import Text.StringTemplate
import System.FilePath.Posix
import System.IO (stderr, hPutStrLn)

import Generate.Types
import Options
import Parser
\end{code}

\subsection{Indentation and Identifier Generation}
\begin{code}
indentString = "  "
nl = toEnum 0x0a :: Char
-- |Simple text indenter.
indentText :: Int -> String -> String
indentText depth input =
  let expanded = concat $ take depth $ repeat indentString
      replaceChar c | c == nl = [nl] ++ expanded
                    | otherwise = [c]
  in expanded ++ (concatMap replaceChar input)

-- |Convert camelCase to hyphen-separated (fooBar -> foo-bar)
hyphenSeparateHumps str =
  let convertChar c = if isAlpha c && isUpper c
                      then ['-', toLower c]
                      else [c]
  in if length str > 0
     then concat $ [(toLower $ head str)] : map convertChar (tail str)
     else []
\end{code}

\subsection{Type Printing}
The parsed representation of the input is quite verbose, so we have to
spend a good amount of code to pull the desired properties from the
rest.

\begin{code}
-- Printers for different parts of the input grammar.  By "Print", we
-- mean return a printable string value.  Some accessors also exist, that 
-- return another type.  They have verb prefixes.

-- |Print the type specified
typeName :: Type -> String
typeName (Predefined (AnyType)) = "any"
typeName (Predefined (NumberType)) = "number"
typeName (Predefined (BooleanType)) = "boolean"
typeName (Predefined (StringType)) = "string"
typeName (Predefined (VoidType)) = "void"
typeName (TypeReference (TypeRef (TypeName _ nm) Nothing)) = nm
typeName (TypeReference (TypeRef (TypeName _ nm) (Just xs))) =
  nm ++ "<" ++ (intercalate "," $ map typeName xs) ++ ">"
typeName (ObjectType ty) = "object" 
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

-- |Many Types are optional.  Default to 'any' if they're not present. 
maybeType :: Maybe Type -> String
maybeType ty = if isJust ty 
                    then typeName $ fromJust ty
                    else "any"

-- |Returns an inner type if the outer type is a Promise<inner>, otherwise it 
-- returns arg.
stripPromise :: Maybe Type -> Maybe Type
stripPromise (Just (TypeReference (TypeRef (TypeName _ "Promise") (Just [ty])))) = Just ty
stripPromise other = other

getParamType :: Parameter -> Maybe Type
getParamType (RequiredOrOptionalParameter _ _ _ ty) = ty
getParamType (RestParameter _ ty) = ty

-- |Print the type of a parameter
paramType :: Parameter -> String
paramType = maybeType . getParamType

-- |Print the return type, or  nothign at all.
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
          ("signature", nm ++ "(" ++
                        (concat $ intersperse ", " $ map paramName params) ++ ")" ++
                        (if length signatureReturn > 0
                         then ": " ++ signatureReturn ++ " " else "")),
          ("invocation", if isJust ipc 
                           then generateInterfaceIPCCall (fromJust ipc) depth nm params
                           else "// write your implementation of " ++ nm ++ " here")] template
      generateInterfaceBody' (_, _) = ""  -- only produce code for method signatures.
      TypeBody decls = declBody
  in indentText depth $ intercalate [nl] $ map generateInterfaceBody' decls

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
% $
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
data FreedomApp = App { script :: T.Text } deriving (Show, Eq)
data FreedomConstraints = Constraints { isolation :: T.Text } deriving (Show, Eq)
data FreedomAPIEntry = ApiEntry 
    { apiType :: T.Text
    , apiValue :: [T.Text]
    , apiRet :: [T.Text] 
    } deriving (Show, Eq)

data FreedomAPI = Api { entries :: [(T.Text, [(T.Text, FreedomAPIEntry)])] } deriving (Show, Eq)
data FreedomManifest = Manifest 
    { name :: T.Text
    , description :: T.Text
    , app :: FreedomApp
    , constraints :: FreedomConstraints
    , provides :: [T.Text]
    , api :: FreedomAPI
    , dependencies :: [T.Text]
    , permissions :: [T.Text]
    } deriving (Show, Eq)

$(TH.deriveJSON TH.defaultOptions ''FreedomApp)
$(TH.deriveJSON TH.defaultOptions ''FreedomConstraints)
$(TH.deriveJSON TH.defaultOptions { TH.fieldLabelModifier = 
                                     \f -> map toLower $ drop 3 f }
                                   ''FreedomAPIEntry)
$(TH.deriveJSON TH.defaultOptions ''FreedomManifest)

-- |FreedomAPI uses custom keys, so do this by hand.
instance FromJSON FreedomAPI where
    parseJSON (Object v) = 
        let readEntry (k, obj) = 
                let parsed = fromJSON obj
                    interpret (Error _) = []
                    interpret (Success s) = s
                in (k, interpret parsed) :: (T.Text, [FreedomAPIEntry])
            allEntries = map readEntry $ toList v
        in mzero -- Api <$> (toList v <$> ZipList readEntry) -- FIXME
    parseJSON _ = mzero

instance ToJSON FreedomAPI where
    toJSON api = let serializeMember (mem, body) = (mem, toJSON body)
                     serializeAPI (api, mems) = (api, object $ map serializeMember mems)
                 in object $ map serializeAPI $ entries api
                                                                    
\end{code}
% $

\paragraph{Method Generation}
The code to generate each method is pretty simple: fill in a
\ident{FreedomAPI} for each exported interface, and within each, a
\ident{FreedomAPIEntry} for each declared method.

Sadly we have to pattern match two-deep here, due to the verbose AST
generated from the parser.
\begin{code}

-- |Match input types against what Freedom allows, and expand ...rest
-- arguments into 'optRestArgCount' args.
generateJSONMethodArgs :: Options -> [Parameter] -> [String]
generateJSONMethodArgs opts params =
    let translateType :: Type -> String
        translateType t@(Predefined (NumberType)) = typeName t
        translateType t@(Predefined (BooleanType)) = typeName t
        translateType t@(Predefined (StringType)) = typeName t
        translateType (TypeReference (TypeRef (TypeName _ "Blob") Nothing)) = "blob"
        translateType (TypeReference (TypeRef (TypeName _ "ArrayBuffer") Nothing)) = "buffer"
        translateType _ = "object"
        -- ^ We should try to catch as many bad inputs in the Analyzer.  It's too late here, 
        -- we've already committed to generation by this point.
        isRest (RestParameter _ _) = True
        isRest _ = False
        lastArgIsRest = (length params > 0) && (isRest $ last params)
        defaultType = (Predefined (StringType))
        getArgTypes dfl ts = map (\a -> maybe dfl id (getParamType a)) ts
        argList = if lastArgIsRest
                  then let totalArgs = max (optRestArgCount opts) (length params)
                           restArgType = maybe defaultType id $ getParamType $ last params
                           realArgTypes = getArgTypes restArgType params
                           syntheticArgs = repeat restArgType
                       in take totalArgs $ realArgTypes ++ syntheticArgs
                  else let defaultType = (Predefined (StringType))
                       in getArgTypes defaultType params
    in map translateType argList

generateJSONMethod :: Options -> String -> ParameterListAndReturnType -> (T.Text, FreedomAPIEntry)
generateJSONMethod opts nm (ParameterListAndReturnType _ params ret) =
    let paramTypes = generateJSONMethodArgs opts params
        retType = if isJust ret then [typeName . fromJust . stripPromise $ ret] else []
    in (T.pack nm, ApiEntry { apiType = "method", 
                              apiValue = map T.pack paramTypes, 
                              apiRet = map T.pack retType })

generateJSONApis :: Options -> DeclarationElement -> (T.Text, [(T.Text, FreedomAPIEntry)])
generateJSONApis opts (InterfaceDeclaration _ _ iface@(Interface _ name _ _ body)) =
    let (TypeBody bodymems) = body
        makeEntry (_,(MethodSignature nm _ params)) = Just $ generateJSONMethod opts nm params
        makeEntry _ = Nothing
    in (T.pack name, mapMaybe makeEntry bodymems)

generateJSONApi :: Options -> [DeclarationElement] -> FreedomAPI
generateJSONApi opts decls =
    let processDecl decl = 
            let iface = exportedInterfaceName decl
            in if isJust iface
               then Just $ generateJSONApis opts decl
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
generateJson :: Options -> FilePath -> Bool -> [DeclarationElement] -> IO ()
generateJson opts path merge decls = do
  let firstIface = head $ mapMaybe exportedInterfaceName decls
      moduleName = hyphenSeparateHumps firstIface
      dflSkeleton = Manifest 
                    { name = T.pack moduleName
                    , description = T.pack ""
                    , app = App { script = T.pack $ moduleName ++ ".js" }
                    , constraints = Constraints { isolation = T.pack "never" }
                    , provides = [T.pack firstIface]
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
  let result = skel { api = generateJSONApi opts decls }
      manifestKeyOrder = ["name", "description", "app", "constraints", 
                          "provides", "api", "dependencies", "permissions", 
                          "type", "value", "ret"]
      config = Config { confIndent = 4, 
                        confCompare = keyOrder manifestKeyOrder `mappend` 
                                       comparing T.length }
  BL.writeFile path $ encodePretty' config result
\end{code}

\subsection{Driver}
\begin{code}
-- |Primary driver for generating typescript code.
generateTS :: Options -> FilePath -> [DeclarationElement] -> IO ()
generateTS options sourceDir decls = do
  let ipc = optIPC options
      exportedInterfaces = mapMaybe exportedInterfaceName decls
      prefix = [nl, nl, '>', ' ']                          
      print s = putStrLn (prefix ++ s)
  if length exportedInterfaces > 0
  then do let generatedFilenameBase = sourceDir </> (head exportedInterfaces)
          print $ "Parsed input AST: " ++ (
                   intercalate (nl : "    ") $ map show decls)
          print $ "Parsed input: " ++ (
                   intercalate [nl] $ map (\d -> renderDeclarationSourceFile [d]) decls)
          case ipc of
            FreedomMessaging ->  do 
                     print $ "Outputting to files " ++ generatedFilenameBase ++ "_(stub|skel).ts"
                     let stubText = concatMap (generateInterface (Just ipc) 1) decls
                         skelText = concatMap (generateInterface Nothing 1) decls
                     writeFile (generatedFilenameBase ++ "_stub.ts") stubText
                     writeFile (generatedFilenameBase ++ "_skel.ts") skelText
            FreedomJSON -> do
                     print $ "Output to files " ++ generatedFilenameBase ++ "_stub.ts and " ++
                           generatedFilenameBase ++ ".json."
                     let skelText = concatMap (generateInterface Nothing 1) decls
                     generateJson options (generatedFilenameBase ++ ".json") False decls
                     writeFile (generatedFilenameBase ++ "_skel.ts") skelText
  else hPutStrLn stderr "> Failure.  No exported interfaces."
\end{code}


