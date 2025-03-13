-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/03/13 23:44:30 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Parser where

import Text.Parsec as P
import Control.Monad
import qualified Data.Set as S

type CProgramAST = [FunctionDefine]

type AsmProgramAST = [AsmFunctionDefine]

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

data AsmFunctionDefine = AsmFunctionDefine
  {
    asmFuncName :: String,
    instructions :: [Instruction]
  }
  deriving (Show, Eq)

data Instruction = Mov {src :: Operand, dst :: Operand}
  | Ret
  deriving (Show, Eq)

data Operand = Imm Int | Register String
  deriving (Show, Eq)

spaceNlTabParser :: ParsecT String u IO ()
spaceNlTabParser = void (space <|> newline <|> tab)

ucLex :: ParsecT String u IO Char
ucLex = char '_'

slashLex :: ParsecT String u IO Char
slashLex = char '/'

doubleSlashLex :: ParsecT String u IO [Char]
doubleSlashLex = (:) <$> slashLex <*> (pure <$> slashLex)

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

fileParser :: (Ord u, Monoid u) => ParsecT String (S.Set u) IO CProgramAST
fileParser = many (try functionDefineParser)
  >>= (\funcs -> skipMany spaceNlTabParser >> eof >> pure funcs)

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

notNewLine :: ParsecT String u IO ()
notNewLine = do
  c <- anyChar
  guard $ c /= '\n'
  pure ()

asteriskLex :: ParsecT String u IO Char
asteriskLex = char '*'

functionDefineParser :: (Ord u, Monoid u) => ParsecT String (S.Set u) IO FunctionDefine
functionDefineParser = do
  modifyState $ S.insert mempty
  retType <- identifierParser
  fName <- identifierParser
  argList <- between openPParser closePParser $ try argListParser
  statments <- between openCurParser closeCurParser $ many $ try returnStatParser
  pure $ FunctionDefine retType fName argList statments

cASTToAsmAST :: CProgramAST -> AsmProgramAST
cASTToAsmAST = map cFuncDefineToAsmFuncDefine

cFuncDefineToAsmFuncDefine :: FunctionDefine -> AsmFunctionDefine
cFuncDefineToAsmFuncDefine fd =
  AsmFunctionDefine (funName fd) $ 
    concatMap cStatmentToAsmInstructions $ body fd

evalExpr :: Expr -> Operand
evalExpr (Constant s) = Imm $ read s
evalExpr _ = Imm 0

cStatmentToAsmInstructions :: Statment -> [Instruction]
cStatmentToAsmInstructions s =
  case s of
    Return expr -> [Mov (evalExpr expr)  (Register "eax"), Ret]
    _ -> []

noExecutableStackString :: String
noExecutableStackString = "\t.section\t.note.GNU-stack,\"\",@progbits\n"

asmProgramASTToAsm :: [AsmFunctionDefine] -> String
asmProgramASTToAsm = unlines . map asmFunctionDefineToStr

operandToStr :: Operand -> String
operandToStr (Register s) = "%" ++ s
operandToStr (Imm i) = "$" ++ show i

instructionToStr :: Instruction -> String
instructionToStr Ret = "ret\n"
instructionToStr (Mov s d) = "movl " ++ operandToStr s ++ ", " ++ operandToStr d ++ "\n"

asmFunctionDefineToStr :: AsmFunctionDefine -> String
asmFunctionDefineToStr (AsmFunctionDefine fname instrs) =
  "\t.globl\t" ++ fname ++ "\n" ++ fname ++ ":\n"
    ++ concatMap (("\t" ++) . instructionToStr) instrs

-- notAsterisk :: ParsecT String u IO ()
-- notAsterisk = do
--   c <- anyChar
--   guard $ c /= '*'
--   pure ()

-- commentParser :: ParsecT String u IO ()
-- commentParser =
--   void $ doubleSlashLex
--     >> skipMany (try notNewLine)
--     >> try newline

-- commentBlockStartParser :: ParsecT String u IO ()
-- commentBlockStartParser = void $ slashLex >> asteriskLex

-- commentBlockEndParser :: ParsecT String u IO ()
-- commentBlockEndParser = void $ asteriskLex >> slashLex

-- commentBlockParser :: ParsecT String u IO ()
-- commentBlockParser =
--   void $ commentBlockStartParser
--     >> skipMany (try notAsterisk)
--     >> commentBlockEndParser
