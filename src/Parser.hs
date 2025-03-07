-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/03/07 12:58:38 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Parser where

import Text.Parsec as P
import qualified Options.Applicative as O
import Control.Monad

-- ??
data Token = KeyInt 
  | KeyVoid
  | KeyReturn
  | OpenP
  | CloseP
  | OpenCur
  | CloseCur
  | SemiCol
  | Ident String
  | IValue Int
  | DValue Double
  deriving (Show, Eq)

type Program = [FunctionDefine]

data FunctionDefine = FunctionDefine
  {
    returnType :: String,
    funName :: String,
    inputArgs :: [InputArgPair],
    body :: [Statment]
  }
  deriving (Show, Eq)

data InputArgPair = ArgPair 
  {
    dataType :: String,
    varName :: String
  }
  deriving (Show, Eq)

data Statment = VariableDecl
  | Return Expr
  deriving (Show, Eq)

-- ??
data DataType = SolidType BuildinDataType
  | JustVoid
  deriving (Show, Eq)

-- ??
data BuildinDataType = IntType
  | DoubleType
  deriving (Show, Eq)

data Expr = Constant String
  | Variable String
  | FunctionCall
  deriving (Show, Eq)

-- ??
data Number = IntValue String
  | DoubleValue String
  deriving (Show, Eq)

-- data Function = Function
--   {
--     returnType :: DataType,
--     name :: String

--   }
--   deriving (Show, Eq)

spaceNlTabParser :: ParsecT String u IO Char
spaceNlTabParser = space <|> newline <|> tab

ucLex :: ParsecT String u IO Char
ucLex = char '_'

symbolExtract :: ParsecT String u IO [Char]
symbolExtract = (:) <$> (letter <|> ucLex) <*> many (alphaNum <|> ucLex)

buildKeywordParser :: Eq b => b -> ParsecT String u IO b -> ParsecT String u IO b
buildKeywordParser k p = do
  skipMany spaceNlTabParser
  symbol <- p
  guard $ symbol == k
  pure symbol

openPParser :: ParsecT String u IO String
openPParser = pure <$> buildKeywordParser '(' anyChar

closePParser :: ParsecT String u IO String
closePParser = pure <$> buildKeywordParser ')' anyChar

openCurParser :: ParsecT String u IO String
openCurParser = pure <$> buildKeywordParser '{' anyChar

closeCurParser :: ParsecT String u IO String
closeCurParser = pure <$> buildKeywordParser '}' anyChar

semiColParser :: ParsecT String u IO String
semiColParser = pure <$> buildKeywordParser ';' anyChar

commaParser :: ParsecT String u IO String
commaParser = pure <$> buildKeywordParser ',' anyChar

keyIntParser :: ParsecT String u IO String
keyIntParser = buildKeywordParser "int" symbolExtract

keyVoidParser :: ParsecT String u IO String
keyVoidParser = buildKeywordParser "void" symbolExtract

keyReturnParser :: ParsecT String u IO String
keyReturnParser = buildKeywordParser "return" symbolExtract

keywordParser :: ParsecT String u IO String
keywordParser = try keyIntParser
  <|> try keyVoidParser
  <|> try keyReturnParser

-- buildinDataTypeParser :: ParsecT String u IO DataType

keyCharParser :: ParsecT String u IO String
keyCharParser = try openPParser
  <|> try closePParser
  <|> try openCurParser
  <|> try closeCurParser
  <|> try semiColParser

keySymbolParser :: ParsecT String u IO String
keySymbolParser = keywordParser <|> keyCharParser

identifierParser :: ParsecT String u IO String
identifierParser = skipMany spaceNlTabParser >> symbolExtract

intParser :: ParsecT String u IO String
intParser = skipMany spaceNlTabParser >> many1 digit

tokenParser :: ParsecT String u IO String
tokenParser = skipMany spaceNlTabParser
  >> (try keySymbolParser <|> identifierParser <|> intParser)

fileParser :: ParsecT String u IO [String]
fileParser = many (try tokenParser)
  >>= (\toks -> skipMany spaceNlTabParser >> eof >> pure toks)

exprParser :: ParsecT String u IO Expr
exprParser = Constant <$> intParser

returnStatParser :: ParsecT String u IO Statment
returnStatParser = do
  _ <- keyReturnParser
  -- optional openPParser
  num <- intParser
  -- optional closePParser
  _ <- semiColParser
  pure $ Return $ Constant num

argPairParser :: ParsecT String u IO InputArgPair
argPairParser = ArgPair <$> identifierParser <*> identifierParser

argListParser :: ParsecT String u IO [InputArgPair]
argListParser = try (keyVoidParser >> pure []) <|> sepBy argPairParser commaParser

functionDefineParser :: ParsecT String u IO FunctionDefine
functionDefineParser = do
  retType <- identifierParser
  fName <- identifierParser
  argList <- between openPParser closePParser argListParser
  statments <- between openCurParser closeCurParser $ many returnStatParser
  -- statments <- pure []
  -- statments <- pure <$> returnStatParser
  pure $ FunctionDefine retType fName argList statments

-- dataTypeParser :: ParsecT String u IO Statment
-- dataTypeParser = do
  


-- functionBodyParser :: ParsecT String u IO FunctionDefine
-- functionBodyParser = do
--   dataType <- 

-- buildKeywordParser :: (Monad m, O.Alternative m, Eq a) => a -> b -> m a -> m b
-- buildKeywordParser k t p = do
--   symbol <- p
--   guard $ symbol == k
--   pure t

-- openPParser :: ParsecT String u IO Token
-- openPParser = buildKeywordParser '(' OpenP anyChar

-- closePParser :: ParsecT String u IO Token
-- closePParser = buildKeywordParser ')' CloseP anyChar

-- openCurParser :: ParsecT String u IO Token
-- openCurParser = buildKeywordParser '{' OpenCur anyChar

-- closeCurParser :: ParsecT String u IO Token
-- closeCurParser = buildKeywordParser '}' CloseCur anyChar

-- semiColParser :: ParsecT String u IO Token
-- semiColParser = buildKeywordParser ';' SemiCol anyChar

-- keyIntParser :: ParsecT String u IO Token
-- keyIntParser = buildKeywordParser "int" KeyInt symbolExtract

-- keyVoidParser :: ParsecT String u IO Token
-- keyVoidParser = buildKeywordParser "void" KeyVoid symbolExtract

-- keyReturnParser :: ParsecT String u IO Token
-- keyReturnParser = buildKeywordParser "return" KeyReturn symbolExtract

-- keywordParser :: ParsecT String u IO Token
-- keywordParser = try keyIntParser
--   <|> try keyVoidParser 
--   <|> try keyReturnParser

-- keyCharParser :: ParsecT String u IO Token
-- keyCharParser = try openPParser
--   <|> try closePParser
--   <|> try openCurParser
--   <|> try closeCurParser
--   <|> try semiColParser

-- keySymbolParser :: ParsecT String u IO Token
-- keySymbolParser = keywordParser <|> keyCharParser

-- identifierParser :: ParsecT String u IO Token
-- identifierParser = Ident <$> symbolExtract

-- intParser :: ParsecT String u IO Token
-- intParser = IValue . read <$> many1 digit

-- tokenParser :: ParsecT String u IO Token
-- tokenParser = skipMany spaceNlTabParser
--   >> (try keySymbolParser <|> identifierParser <|> intParser)

-- fileParser :: ParsecT String u IO [Token]
-- fileParser = many (try tokenParser)
--   >>= (\toks -> skipMany spaceNlTabParser >> eof >> pure toks)