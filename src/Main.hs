-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/02/28 20:33:12 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where

import qualified Options.Applicative as O
import Text.Parsec as P
import Control.Monad
-- import System.IO

data  Args = Args
  {ifiles :: [String],
  doLex :: Bool,
  doParse :: Bool,
  codegen :: Bool}

data  Token = KeyInt 
  | KeyVoid
  | KeyReturn
  | OpenP
  | CloseP
  | OpenCur
  | CloseCur
  | SemiCol
  | Identifier String
  | IValue Int
  | DValue Double
  deriving (Show, Eq)

-- data  Program =

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
identifierParser = Identifier <$> symbolExtract

intParser :: ParsecT String u IO Token
intParser = IValue . read <$> many1 digit

tokenParser :: ParsecT String u IO Token
tokenParser = skipMany spaceNlTabParser
  >> (try keySymbolParser <|> identifierParser <|> intParser)

fileParser :: ParsecT String u IO [Token]
fileParser = many (try tokenParser)
  >>= (\toks -> skipMany spaceNlTabParser >> eof >> pure toks)

main :: IO ()
main = do
  printArgs =<< O.execParser
    (O.info (argsParser O.<**> O.helper)
    (O.fullDesc <> O.progDesc "aaaa" <> O.header "bbb"))
  print =<< runParserT fileParser () "" "int int   int; return intt; 234 void3;;;  "
  print =<< runParserT fileParser () "" "234 3 ;"
  print =<< runParserT fileParser () "" ") return"
  print =<< runParserT fileParser () "" "return   "
  print =<< runParserT fileParser () "" "int main(void){return (3);}"

printArgs :: Args -> IO ()
printArgs args = do
  print $ ifiles args
  print $ doLex args
  print $ doParse args
  print $ codegen args