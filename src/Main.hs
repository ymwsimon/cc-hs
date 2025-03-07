-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/03/07 12:57:32 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where

import qualified Options.Applicative as O
import Text.Parsec as P
import Control.Monad

import Parser
-- import System.IO

data Args = Args
  {
    ifiles :: [String],
    doLex :: Bool,
    doParse :: Bool,
    codegen :: Bool
  }
  deriving (Show, Eq)

argsParser :: O.Parser Args
argsParser = Args
  <$> O.many (O.argument O.str (O.metavar "input files"))
  <*> O.switch (O.long "lex")
  <*> O.switch (O.long "parse")
  <*> O.switch (O.long "codegen")

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
  print =<< runParserT fileParser () "" "int main(void){return (3);}"
  print =<< runParserT functionDefineParser () "" "int main(void){return 3;}"
  print =<< runParserT returnStatParser () "" "  return   1  ;"

printArgs :: Args -> IO ()
printArgs args = do
  print $ ifiles args
  print $ doLex args
  print $ doParse args
  print $ codegen args