-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/02/26 19:56:30 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --


module Main where

import Options.Applicative as O
-- import Text.ParserCombinators.ReadP as ReadP
-- import Text.ParserCombinators.Parsec as P
import Text.Parsec as P
import Text.Parsec.Char as PC

data  Args = Args {ifiles :: [String], doLex :: Bool, doParse :: Bool, codegen :: Bool}

data  Token = KeyInt 
              | KeyVoid
              | Return
              | OpenP
              | CloseP
              | OpenCur
              | CloseCur
              | SemiCol
              | Identifier String
              | IValue Int
              | DValue Double
              deriving Show

argsParser :: O.Parser Args
argsParser = Args <$> O.many (argument str (metavar "input files")) <*> switch (long "lex") <*> switch (long "parse") <*> switch (long "codegen")

-- keyIntParser :: ReadP String
keyIntParser = string "int"

-- -- keyVoidParser :: ReadP String
-- keyVoidParser = string "void"

-- returnParser = string "return"

-- openPParser = char '('

-- closePParser = char ')'

-- openCurParser = char '{'

-- closeCurParser = char ')'

-- semiColParser = char ':'

isLower = flip elem ['a'..'z']

isUpper = flip elem ['A'..'Z']

isdigit = flip elem ['0'..'9']

isUnderscore = (== '_')


underscoreParser = char '_'

isSymbolBody c = any ($ c)[isLower, isUpper, isdigit, isUnderscore]

symbolBody = many1 $ letter P.<|> digit P.<|> underscoreParser

isSymbolHead c = any ($ c) [isLower, isUpper, isUnderscore]

symbolHead = letter P.<|> underscoreParser

symbolExtract = (:) <$> symbolHead  <*> symbolBody


-- symbolParser = symbolExtract >>= pure runParser keyIntParser () ""
symbolParser = symbolExtract >> pure KeyInt
-- symbolExtract = (:) <$> satisfy isSymbolHead <*> P.many (satisfy isSymbolBody)

-- testParser = symbolExtract >>= pure keyIntParser
-- symbolParser = Sym <$> 

-- tokenParser = 

main :: IO ()
-- main = execParser (info (argsParser <**> helper) (fullDesc <> progDesc "aaaa" <> header "bbb")) >>= printArgs
main = do
  printArgs =<< execParser (info (argsParser <**> helper) (fullDesc <> progDesc "aaaa" <> header "bbb"))
  -- print $ runParser symbolExtract () ""
  -- print $ readP_to_S symbolExtract "____1"

printArgs :: Args -> IO ()
printArgs args = 
  do
    print $ ifiles args
    print $ doLex args
    print $ doParse args
    print $ codegen args