-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/04/02 22:42:44 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where

import qualified Options.Applicative as O
import Text.Parsec as P
import Data.List
import Control.Monad
import System.IO
import qualified Data.Set as S
import Parser
import System.Exit
import System.Process

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

defaultParsecState :: (S.Set String, Int)
defaultParsecState = (S.empty :: S.Set String, lowestPrecedence)

outFileName :: String -> String
outFileName fileName
  | ".c" `isSuffixOf` fileName && length fileName > 2 =
      take (length fileName - 2) fileName ++ ".s"
  | otherwise = ""

readNParse :: FilePath -> IO (Either ParseError [FunctionDefine])
readNParse path =
  if null $ outFileName path
    then do
        putStrLn "Incorrect file name"
        pure $ parse (parserFail "") "" ""
    else do
      (_, Just hout, _, _) <-
        createProcess
          (proc "cc" ["-P", "-E", path])
          { std_out = CreatePipe }
      content <- hGetContents' hout
      putStrLn $ "filename:\n\t" ++ path
      putStrLn $ "content:\n" ++ content
      res <- runParserT fileParser defaultParsecState "" content
      either
        (\parseError -> do
          print parseError
          pure $ Left parseError)
        (\parseOk -> do
          let converted = convertCASTToAsmStr parseOk
          writeFile (outFileName path) converted
          void $ createProcess (proc "cc" [outFileName path])
          putStrLn converted
          pure $ Right parseOk)
        res

main :: IO ()
main = do
  args <- O.execParser
    (O.info (argsParser O.<**> O.helper)
    (O.fullDesc <> O.progDesc "desc" <> O.header "header"))
  res <- mapM readNParse $ ifiles args
  unless (all (either (const False) (const True)) res) exitFailure

printArgs :: Args -> IO ()
printArgs args = do
  print $ ifiles args
  print $ doLex args
  print $ doParse args
  print $ codegen args