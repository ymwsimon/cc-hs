-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/06/13 17:29:18 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where

import qualified Options.Applicative as O
import Text.Parsec as P
import Data.List
import Control.Monad
import System.IO
import qualified Data.Map as M
import Parser
import IR
import Assembly
import Operation
import System.Exit
import System.Process
import Control.Monad.State

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

defaultParsecState :: (M.Map String String, Int)
defaultParsecState = (M.singleton varIdMapKey "1", lowestPrecedence)

outFileName :: String -> String
outFileName fileName
  | ".c" `isSuffixOf` fileName && length fileName > 2 =
      take (length fileName - 2) fileName ++ ".s"
  | otherwise = ""

convertCASTToAsmStr :: CProgramAST -> String
convertCASTToAsmStr =       
  concat
    . (++ [noExecutableStackString])
    . map
      (asmFunctionDefineToStr
        . replacePseudoRegAllocateStackFixDoubleStackOperand)
    . irASTToAsmAST
    . flip evalState (1, 1) . cASTToIrAST

readNParse :: FilePath -> IO (Either ParseError [FunctionDefine])
readNParse path =
  if null $ outFileName path
    then do
        putStrLn "Incorrect file name"
        pure $ parse (parserFail "") "" ""
    else do
      (_, Just hout, _, ppid) <-
        createProcess
          (proc "cc" ["-P", "-E", path])
          { std_out = CreatePipe }
      ppEC <- waitForProcess ppid
      if ppEC /= ExitSuccess
        then do
          putStrLn "Preprocesser fail"
          pure $ parse (parserFail "") "" ""
        else do
          content <- hGetContents' hout
          putStrLn $ "filename:\n\t" ++ path
          putStrLn $ "content:\n" ++ content
          res <- runParserT fileParser defaultParsecState "" content
          either
            (\parseError -> do
              print parseError
              pure $ Left parseError)
            (\parseOk ->
              let converted = convertCASTToAsmStr parseOk in
                do
                  print parseOk
                  putStrLn ""
                  print $ flip evalState (1, 1) $ cASTToIrAST parseOk
                  -- putStrLn ""
                  -- print $ irASTToAsmAST $ flip evalState (1, 1) $ cASTToIrAST parseOk
                  -- putStrLn ""
                  -- print $ map replacePseudoRegAllocateStackFixDoubleStackOperand $ irASTToAsmAST $ flip evalState (1, 1) $ cASTToIrAST parseOk
                  -- putStrLn ""
                  -- print $ irASTToAsmAST $ flip evalState (1, 1) . cASTToIrAST parseOk
                  -- putStrLn converted
                  writeFile (outFileName path) converted
                  (_, _, _, assemblerPid) <- createProcess $ proc "cc" [outFileName path]
                  assemblerEC <- waitForProcess assemblerPid
                  if assemblerEC == ExitSuccess
                    then
                      do
                        putStrLn converted
                        pure $ Right parseOk
                    else
                      pure $ parse (parserFail "") "" "")
            res

main :: IO ()
main = do
  args <- O.execParser
    (O.info (argsParser O.<**> O.helper)
    (O.fullDesc <> O.progDesc "desc" <> O.header "header"))
  res <- mapM readNParse $ ifiles args
  -- unless (all (either (const False) (const True)) res) exitFailure
  (`unless` exitFailure) $ (`all` res) $ either (const False) (const True)

printArgs :: Args -> IO ()
printArgs args = do
  print $ ifiles args
  print $ doLex args
  print $ doParse args
  print $ codegen args