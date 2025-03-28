-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/03/27 20:27:06 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Main where

import qualified Options.Applicative as O
import Text.Parsec as P
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

readNParse :: FilePath -> IO (Either ParseError [FunctionDefine])
readNParse path = do
  (_, Just hout, _, _) <- createProcess (proc "cc" ["-P", "-E", path]) { std_out = CreatePipe }
  content <- hGetContents' hout
  putStrLn $ "filename: " ++ path
  res <- runParserT fileParser (S.empty :: S.Set String, 0) "" content
  either (const (pure ())) print res 
  -- print $ map replacePseudoRegAllocateStackFixDoubleStackOperand
  --   . irASTToAsmAST
  --   . cASTToIrAST
  --   <$> res
  -- putStrLn $ concat $
  --   either
  --     (const [""])
  --     ((++ [noExecutableStackString])
  --       . map (asmFunctionDefineToStr . replacePseudoRegAllocateStackFixDoubleStackOperand)
  --       . irASTToAsmAST
  --       . cASTToIrAST)
  --     res
  pure res

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