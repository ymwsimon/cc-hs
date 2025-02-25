-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/02/25 21:31:18 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --


module Main where

import Options.Applicative as O
import Text.ParserCombinators.ReadP

data  Args = Args {ifiles :: [String], doLex :: Bool, doParse :: Bool, codegen :: Bool}

data  Token = KeyInt 
              | KeyVoid
              | Return
              | OpenP
              | CloseP
              | OpenCur
              | CloseCur
              | SemiCol
              | Sym String
              | IValue Int
              deriving Show

argsParser :: Parser Args
argsParser = Args <$> O.many (argument str (metavar "input files")) <*> switch (long "lex") <*> switch (long "parse") <*> switch (long "codegen")

keyIntParser :: Text.ParserCombinators.ReadP.ReadP String
keyIntParser = string "int"

keyVoidParser :: ReadP String
keyVoidParser = string "void"

returnParser = string "return"

openPParser = char '('

closePParser = char ')'

openCurParser = char '{'

closeCurParser = char ')'

semiColParser = char ':'

-- symbolParser = Sym <$> 

-- tokenParser = 

main :: IO ()
-- main = execParser (info (argsParser <**> helper) (fullDesc <> progDesc "aaaa" <> header "bbb")) >>= printArgs
main = printArgs =<< execParser (info (argsParser <**> helper) (fullDesc <> progDesc "aaaa" <> header "bbb"))

printArgs :: Args -> IO ()
printArgs args = 
  do
    print $ ifiles args
    print $ doLex args
    print $ doParse args
    print $ codegen args