-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/06/30 13:58:48 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Options.Applicative as O
import Text.Parsec as P
import Data.List
import Control.Monad
import System.IO
import Parser
import IR
import Assembly
import System.Exit
import System.Process
import Control.Monad.State

data Args = Args
  {
    ifiles :: [String],
    doLex :: Bool,
    doParse :: Bool,
    codegen :: Bool,
    objOnly :: Bool
  }
  deriving (Show, Eq)

argsParser :: O.Parser Args
argsParser = Args
  <$> O.many (O.argument O.str (O.metavar "input files"))
  <*> O.switch (O.long "lex")
  <*> O.switch (O.long "parse")
  <*> O.switch (O.long "codegen")
  <*> O.switch (O.short 'c')

outAsmFileName :: String -> String
outAsmFileName fileName
  | ".c" `isSuffixOf` fileName && length fileName > 2 =
      take (length fileName - 2) fileName ++ ".s"
  | otherwise = ""

outExeFileName :: String -> String
outExeFileName fileName
  | ".c" `isSuffixOf` fileName && length fileName > 2 =
      take (length fileName - 2) fileName
  | otherwise = ""

outObjFileName :: String -> String
outObjFileName fileName
  | ".c" `isSuffixOf` fileName && length fileName > 2 =
      take (length fileName - 2) fileName ++ ".o"
  | otherwise = ""

convertCASTToAsm :: CProgramAST -> [AsmFunctionDefine]
convertCASTToAsm = 
    irASTToAsmAST
    . flip evalState (1, 1) . cASTToIrAST

convertCASTToAsmStr :: CProgramAST -> String
convertCASTToAsmStr =       
  concat
    . (++ [noExecutableStackString])
    . map
      (asmFunctionDefineToStr
        . replacePseudoRegAllocateStackFixDoubleStackOperand)
    . irASTToAsmAST
    . flip evalState (1, 1) . cASTToIrAST

parseOkAct :: Args -> String -> [Declaration] -> IO (Either ParseError [Declaration])
parseOkAct args path parseOk =
  do
    print parseOk
    putStrLn ""
    print $ flip evalState (1, 1) $ cASTToIrAST parseOk
    putStrLn ""
    let fdsBlock = map (\case
            FunctionDeclaration _ _ _ (Just bl) _ _ -> unBlock bl
            _ -> []) parseOk
        labelCheckRes = labelCheck fdsBlock in
      case labelCheckRes of
        Left errs -> putStr (unlines errs) >> pure (parse (parserFail "") "" "")
        Right labelMap -> do
          print $ convertCASTToAsm $ updateGotoLabel parseOk labelMap
          let updatedLabel = updateGotoLabel parseOk labelMap
              converted = convertCASTToAsmStr updatedLabel
          writeFile (outAsmFileName path) converted
          (_, _, _, assemblerPid) <- if objOnly args
            then createProcess $ proc "cc" [outAsmFileName path, "-c", "-o", outObjFileName path]
            else createProcess $ proc "cc" [outAsmFileName path, "-o", outExeFileName path]
          assemblerEC <- waitForProcess assemblerPid
          if assemblerEC == ExitSuccess
            then
              do
                putStrLn converted
                pure $ Right updatedLabel
            else
              pure $ parse (parserFail "") "" ""

readNParse :: Args -> FilePath -> IO (Either ParseError [Declaration])
readNParse args path =
  if null $ outAsmFileName path
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
            (parseOkAct args path)
            res

main :: IO ()
main = do
  args <- O.execParser
    (O.info (argsParser O.<**> O.helper)
    (O.fullDesc <> O.progDesc "desc" <> O.header "header"))
  res <- mapM (readNParse args) $ ifiles args
  -- unless (all (either (const False) (const True)) res) exitFailure
  (`unless` exitFailure) $ (`all` res) $ either (const False) (const True)

printArgs :: Args -> IO ()
printArgs args = do
  print $ ifiles args
  print $ doLex args
  print $ doParse args
  print $ codegen args