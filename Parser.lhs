% Local Variables:
% mode: latex 
% mmm-classes: literate-haskell-latex
% End:

\section{Parser}
The parser uses the existing \emph{language-typescript} package and
then converts this input into its own data structures.

\lstset{firstnumber=1}
\begin{code}
module Parser (parseDeclarations, Type(..), Method(..), Class(..)) where
import qualified Language.TypeScript as TS
import Text.ParserCombinators.Parsec.Prim (parse)
import Data.Either.Unwrap
import Data.List (insert)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import qualified Language.TypeScript.Types as T

import Options
\end{code}

\subsection{Intermediate Representation}
The parser puts out an \emph{intermediate representation} that 
For now, we can keep with simple records.  If we need more abstract
methods in analysis or generation, we can consider lenses.  It lets us decide 
interpretation policy within the parser and semantic policy within the 
analyzer.  Note that there's no symbol table or attempt to half-implement one 
-- types are names or parameterized names only, and we make no attempt to save 
their definition.  So far, that level of interpretation hasn't been necessary.

\begin{code}

data Type = Type { typeName :: String
                 , typeArgs :: [Type]
                 } deriving (Eq, Show)

data Method = Method { methodName :: String
                     , methodParams :: [(String, Type)]
                     , methodRest :: Maybe (String, Type)
                     , methodReturn :: Maybe Type
                     } deriving (Eq, Show)

-- | Whether a type is marked as an interface or class.  Generally we'll refer 
-- to classes or interfaces identically as classes.  The difference only matters
-- in parsing and code generation.
data ClassTag = TagInterface | TagClass deriving (Eq, Show)

data Class = Class { className :: String
                   , classModuleName :: Maybe String
                   , classExported :: Bool
                   , classMethods :: [Method]
                   , classConstructor :: Maybe Method
                   , classTag :: ClassTag
                   } deriving (Eq, Show)

pullList :: Maybe [a] -> [a]
pullList (Just s) = s
pullList Nothing = []

pullMaybeHead :: [a] -> Maybe a
pullMaybeHead [] = Nothing
pullMaybeHead (x:xs) = Just x

simpleType :: String -> Type
simpleType nm = Type { typeName = nm, typeArgs = [] }

list :: a -> [a]
list x = [x]

convertType :: T.Type -> Maybe Type
convertType (T.Predefined (T.AnyType)) = Just $ simpleType "any"
convertType (T.Predefined (T.NumberType)) = Just $ simpleType "number"
convertType (T.Predefined (T.BooleanType)) = Just $ simpleType "boolean"
convertType (T.Predefined (T.StringType)) = Just $ simpleType "string"
convertType (T.Predefined (T.VoidType)) = Nothing
convertType (T.TypeReference (T.TypeRef (T.TypeName _ nm) Nothing)) = 
  Just $ simpleType nm
convertType (T.TypeReference (T.TypeRef (T.TypeName _ nm) (Just xs))) = 
  Just $ (simpleType nm) { typeArgs = mapMaybe convertType xs }
convertType (T.ObjectType ty) = Just $ simpleType "object"
convertType (T.ArrayType ty) = 
  Just $ (simpleType "Array") { typeArgs = maybe [] list (convertType ty) }
convertType (T.FunctionType _ _ _) = Nothing
convertType (T.ConstructorType _ _ _) = Nothing

convertMaybeType :: Maybe T.Type -> Maybe Type
convertMaybeType mty = maybe Nothing convertType mty

convertSignature :: Options -> T.TypeMember -> Maybe Method
convertSignature opts (T.MethodSignature name _ (
                          T.ParameterListAndReturnType _ params rettype)) = 
  let isRest (T.RestParameter _ _) = True
      isRest _ = False
      defaultType = simpleType "string"
      paramType (T.RequiredOrOptionalParameter _ name _ ty) = 
        (name, fromMaybe defaultType $ convertMaybeType ty)
      paramType (T.RestParameter name ty) =
        (name, fromMaybe defaultType $ convertMaybeType ty)
      -- convert all the regular args
      regularArgs = map paramType $ filter (not . isRest) params
      -- and the 'rest' args
      restArg = pullMaybeHead $ map paramType $ filter isRest params
  in Just $ Method { methodName = name
                   , methodParams = regularArgs
                   , methodRest = restArg
                   , methodReturn = convertMaybeType rettype }

convertSignature opts _ = Nothing

-- |TypeParameters are template args (in C++ parlance) and the typerefs are 
-- implemented/extended interfaces
convertInterface opts exported (T.Interface _ nm mparams mrefs (T.TypeBody memlist)) =
  let params = pullList mparams
      refs = pullList mrefs
      (_, mems) = unzip memlist
      convMember sig@(T.MethodSignature nm _ _) = convertSignature opts sig
      convMember _ = Nothing
  in Class { className = nm
           , classModuleName = Nothing
           , classExported = exported
           , classMethods = mapMaybe convMember mems
           , classConstructor = Nothing
           , classTag = TagInterface }

convertDecl :: Options -> T.DeclarationElement -> Maybe ([String], Class)
convertDecl opts (T.InterfaceDeclaration _  exported iface) = 
  Just ([], convertInterface opts (isJust exported) iface)
convertDecl _ (T.ImportDeclaration _ _ _) = Nothing
convertDecl _ (T.ExportDeclaration _) = Nothing
convertDecl _ (T.ExternalImportDeclaration _ _ _) = Nothing
convertDecl _ (T.AmbientDeclaration _ exported ambient) = Nothing

convertInput :: Options -> [T.DeclarationElement] -> ([String], [Class])
convertInput opts decls = let conversions = mapMaybe (convertDecl opts) decls
                              (warnings, classes) = unzip conversions
                          in (concat warnings, classes)

-- |Parse input text and return the parsed form, or output parse
-- errors and return an empty list.
parseDeclarations :: Options -> String -> String -> IO ([String], [Class])
parseDeclarations opts filename text =
  do let parseResult = parse TS.declarationSourceFile filename text
     if isRight parseResult
        then return $ convertInput opts $ fromRight parseResult
        else do putStrLn $ show $ fromLeft parseResult
                return ([], [])
\end{code}
