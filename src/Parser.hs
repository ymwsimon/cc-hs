-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/03/11 17:01:22 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Parser where

-- import qualified Options.Applicative as O
import Text.Parsec as P
import Control.Monad
import Data.Set as S

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

data Expr = Constant String
  | Variable String
  | FunctionCall
  deriving (Show, Eq)

data Operand = Imm Int
  deriving (Show, Eq)

data Register = Register String
  deriving (Show, Eq)


-- ??
-- data Token = KeyInt 
--   | KeyVoid
--   | KeyReturn
--   | OpenP
--   | CloseP
--   | OpenCur
--   | CloseCur
--   | SemiCol
--   | Ident String
--   | IValue Int
--   | DValue Double
--   deriving (Show, Eq)

-- ??
-- data DataType = SolidType BuildinDataType
--   | JustVoid
--   deriving (Show, Eq)

-- ??
-- data BuildinDataType = IntType
--   | DoubleType
--   deriving (Show, Eq)

-- ??
-- data Number = IntValue String
--   | DoubleValue String
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
  openP <- try openPParser <|> pure []
  num <- intParser
  _ <- case openP of
          "(" -> closePParser
          _ -> pure []
  _ <- semiColParser
  pure $ Return $ Constant num

argPairParser :: ParsecT String u IO InputArgPair
argPairParser = ArgPair <$> identifierParser <*> identifierParser

argListParser :: ParsecT String u IO [InputArgPair]
argListParser = do
  res <- try keyVoidParser <|> pure []
  case res of
    "void" -> pure []
    _ -> try (sepBy argPairParser (try commaParser)) <|> pure []

-- functionDefineParser :: ParsecT String u IO FunctionDefine
functionDefineParser :: (Ord u, Monoid u) => ParsecT String (Set u) IO FunctionDefine
functionDefineParser = do
  modifyState $ S.insert mempty
  retType <- identifierParser
  fName <- identifierParser
  argList <- between openPParser closePParser $ try argListParser
  statments <- between openCurParser closeCurParser $ many $ try returnStatParser
  pure $ FunctionDefine retType fName argList statments
