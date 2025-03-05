-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/03/05 23:12:45 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where

import qualified Options.Applicative as O
import Text.Parsec as P
import Control.Monad
-- import System.IO

data Args = Args
  {
    ifiles :: [String],
    doLex :: Bool,
    doParse :: Bool,
    codegen :: Bool
  }
  deriving (Show, Eq)

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
    returnType :: DataType,
    funName :: String,
    inputArgs :: [InputArgPair],
    body :: [Statment]
  }
  deriving (Show, Eq)

data InputArgPair = ArgPair 
  {
    dataType :: BuildinDataType,
    varName :: String
  }
  deriving (Show, Eq)

data Statment = VariableDecl
  | Return Expr
  deriving (Show, Eq)

data DataType = SolidType BuildinDataType
  | JustVoid
  deriving (Show, Eq)

data BuildinDataType = IntType
  | DoubleType
  deriving (Show, Eq)

data Expr = Constant Number
  | Variable String
  | FunctionCall
  deriving (Show, Eq)

data Number = IntValue String
  | DoubleValue String
  deriving (Show, Eq)

argsParser :: O.Parser Args
argsParser = Args
  <$> O.many (O.argument O.str (O.metavar "input files"))
  <*> O.switch (O.long "lex")
  <*> O.switch (O.long "parse")
  <*> O.switch (O.long "codegen")

spaceNlTabParser :: ParsecT String u IO Char
spaceNlTabParser = space <|> newline <|> tab

ucLex :: ParsecT String u IO Char
ucLex = char '_'

symbolExtract :: ParsecT String u IO [Char]
symbolExtract = (:) <$> (letter <|> ucLex) <*> many (alphaNum <|> ucLex)

buildKeywordParserc :: Eq b => b -> ParsecT String u IO b -> ParsecT String u IO b
buildKeywordParserc k p = do
  skipMany spaceNlTabParser
  symbol <- p
  guard $ symbol == k
  pure symbol

openPParserc :: ParsecT String u IO String
openPParserc = pure <$> buildKeywordParserc '(' anyChar

closePParserc :: ParsecT String u IO String
closePParserc = pure <$> buildKeywordParserc ')' anyChar

openCurParserc :: ParsecT String u IO String
openCurParserc = pure <$> buildKeywordParserc '{' anyChar

closeCurParserc :: ParsecT String u IO String
closeCurParserc = pure <$> buildKeywordParserc '}' anyChar

semiColParserc :: ParsecT String u IO String
semiColParserc = pure <$> buildKeywordParserc ';' anyChar

keyIntParserS :: ParsecT String u IO String
keyIntParserS = buildKeywordParserc "int" symbolExtract

keyVoidParserc :: ParsecT String u IO String
keyVoidParserc = buildKeywordParserc "void" symbolExtract

keyReturnParserc :: ParsecT String u IO String
keyReturnParserc = buildKeywordParserc "return" symbolExtract

keywordParserc :: ParsecT String u IO String
keywordParserc = try keyIntParserS
  <|> try keyVoidParserc
  <|> try keyReturnParserc

keyCharParserc :: ParsecT String u IO String
keyCharParserc = try openPParserc
  <|> try closePParserc
  <|> try openCurParserc
  <|> try closeCurParserc
  <|> try semiColParserc

keySymbolParserc :: ParsecT String u IO String
keySymbolParserc = keywordParserc <|> keyCharParserc

identifierParserc :: ParsecT String u IO String
identifierParserc = symbolExtract

intParserc :: ParsecT String u IO String
intParserc = skipMany spaceNlTabParser >> many1 digit

tokenParserc :: ParsecT String u IO String
tokenParserc = skipMany spaceNlTabParser
  >> (try keySymbolParserc <|> identifierParserc <|> intParserc)

fileParserc :: ParsecT String u IO [String]
fileParserc = many (try tokenParserc)
  >>= (\toks -> skipMany spaceNlTabParser >> eof >> pure toks)

exprParser :: ParsecT String u IO Expr
exprParser = Constant . IntValue <$> intParserc

returnStatParser :: ParsecT String u IO Statment
returnStatParser = do
  _ <- keyReturnParserc
  num <- intParserc
  _ <- semiColParserc
  pure $ Return $ Constant $ IntValue num

buildKeywordParser :: (Monad m, O.Alternative m, Eq a) => a -> b -> m a -> m b
buildKeywordParser k t p = do
  symbol <- p
  guard $ symbol == k
  pure t

openPParser :: ParsecT String u IO Token
openPParser = buildKeywordParser '(' OpenP anyChar

closePParser :: ParsecT String u IO Token
closePParser = buildKeywordParser ')' CloseP anyChar

openCurParser :: ParsecT String u IO Token
openCurParser = buildKeywordParser '{' OpenCur anyChar

closeCurParser :: ParsecT String u IO Token
closeCurParser = buildKeywordParser '}' CloseCur anyChar

semiColParser :: ParsecT String u IO Token
semiColParser = buildKeywordParser ';' SemiCol anyChar

keyIntParser :: ParsecT String u IO Token
keyIntParser = buildKeywordParser "int" KeyInt symbolExtract

keyVoidParser :: ParsecT String u IO Token
keyVoidParser = buildKeywordParser "void" KeyVoid symbolExtract

keyReturnParser :: ParsecT String u IO Token
keyReturnParser = buildKeywordParser "return" KeyReturn symbolExtract

keywordParser :: ParsecT String u IO Token
keywordParser = try keyIntParser
  <|> try keyVoidParser 
  <|> try keyReturnParser

keyCharParser :: ParsecT String u IO Token
keyCharParser = try openPParser
  <|> try closePParser
  <|> try openCurParser
  <|> try closeCurParser
  <|> try semiColParser

keySymbolParser :: ParsecT String u IO Token
keySymbolParser = keywordParser <|> keyCharParser

identifierParser :: ParsecT String u IO Token
identifierParser = Ident <$> symbolExtract

intParser :: ParsecT String u IO Token
intParser = IValue . read <$> many1 digit

tokenParser :: ParsecT String u IO Token
tokenParser = skipMany spaceNlTabParser
  >> (try keySymbolParser <|> identifierParser <|> intParser)

fileParser :: ParsecT String u IO [Token]
fileParser = many (try tokenParser)
  >>= (\toks -> skipMany spaceNlTabParser >> eof >> pure toks)

-- programParser = many $ 

-- exprConstantParser :: ParsecT [Token] u IO Expr
-- exprConstantParser =  do
--   num <- satisfy 

-- programParser :: ParsecT [Token] u IO Program
-- programParser = do
  -- pure $ _

-- returnTypeParser = do
--   returnType <- _
--   case returnType of
--     KeyVoid -> pure VoidType
--     KeyInt -> pure IntType
--     _ -> Left "Error"

main :: IO ()
main = do
  printArgs =<< O.execParser
    (O.info (argsParser O.<**> O.helper)
    (O.fullDesc <> O.progDesc "aaaa" <> O.header "bbb"))
  -- print =<< runParserT fileParser () "" "int int   int; return intt; 234 void3;;;  "
  -- print =<< runParserT fileParserc () "" "int int   int; return intt; 234 void3;;;  "
  -- print =<< runParserT fileParserc () "" "234 3 ;"
  -- print =<< runParserT fileParserc () "" ") return"
  -- print =<< runParserT fileParserc () "" "return   "
  -- print =<< runParserT fileParserc () "" "int intmain(voidd){return (3);}"
  print =<< runParserT fileParserc () "" "int main(void){return (3);}"
  print =<< runParserT returnStatParser () "" "  return   1  ;"

printArgs :: Args -> IO ()
printArgs args = do
  print $ ifiles args
  print $ doLex args
  print $ doParse args
  print $ codegen args