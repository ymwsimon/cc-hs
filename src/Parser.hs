-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/03/20 12:08:15 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Parser where

import Text.Parsec as P
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

createSkipSpacesCharParser :: Char -> ParsecT String u IO Char
createSkipSpacesCharParser = (spaces >>) . char

ucLex :: ParsecT String u IO Char
ucLex = createSkipSpacesCharParser '_'

symbolExtract :: ParsecT String u IO String
symbolExtract = do
  firstLetter <- letter <|> try ucLex
  tailLetters <- many (alphaNum <|> try ucLex)
  pure (firstLetter : tailLetters)

createSkipSpacesStringParser :: String -> ParsecT String u IO String
createSkipSpacesStringParser = (spaces >>) . string'

openPParser :: ParsecT String u IO String
openPParser = createSkipSpacesStringParser "("

closePParser :: ParsecT String u IO String
closePParser = createSkipSpacesStringParser ")"

openCurParser :: ParsecT String u IO String
openCurParser = createSkipSpacesStringParser "{"

closeCurParser :: ParsecT String u IO String
closeCurParser = createSkipSpacesStringParser "}"

semiColParser :: ParsecT String u IO String
semiColParser = createSkipSpacesStringParser ";"

commaParser :: ParsecT String u IO String
commaParser = createSkipSpacesStringParser ","

keyIntParser :: ParsecT String u IO String
keyIntParser = createSkipSpacesStringParser "int"

keyVoidParser :: ParsecT String u IO String
keyVoidParser = createSkipSpacesStringParser "void"

keyReturnParser :: ParsecT String u IO String
keyReturnParser = createSkipSpacesStringParser "return"

keywordParser :: ParsecT String u IO String
keywordParser = keyIntParser
  <|> keyVoidParser
  <|> keyReturnParser

keyCharParser :: ParsecT String u IO String
keyCharParser = openPParser
  <|> closePParser
  <|> openCurParser
  <|> closeCurParser
  <|> semiColParser

keySymbolParser :: ParsecT String u IO String
keySymbolParser = keywordParser <|> keyCharParser

identifierParser :: ParsecT String u IO String
identifierParser = spaces >> symbolExtract

intParser :: ParsecT String u IO String
intParser = spaces >> many1 digit

tokenParser :: ParsecT String u IO String
tokenParser = keySymbolParser <|> identifierParser <|> intParser

fileParser :: (Ord u, Monoid u) => ParsecT String (S.Set u) IO CProgramAST
fileParser = manyTill functionDefineParser (try (spaces >> eof))

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

functionDefineParser :: (Ord u, Monoid u) => ParsecT String (S.Set u) IO FunctionDefine
functionDefineParser = do
  modifyState $ S.insert mempty
  -- retType <- identifierParser -- <|> keyIntParser <|> keyVoidParser <|> identifierParser
  retType <- (keyIntParser <|> keyVoidParser) <* notFollowedBy (alphaNum <|> try ucLex)
  fName <- identifierParser
  argList <- between openPParser closePParser $ try argListParser
  _ <- openCurParser
  statments <- manyTill returnStatParser (try closeCurParser)
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

-- spaceNlTabParser :: ParsecT String u IO ()
-- spaceNlTabParser = void (space <|> newline <|> tab)

-- slashLex :: ParsecT String u IO Char
-- slashLex = createSkipSpacesCharParser '/'

-- doubleSlashLex :: ParsecT String u IO String
-- doubleSlashLex = (:) <$> slashLex <*> (pure <$> slashLex)

-- buildKeywordParser :: Eq b => b -> ParsecT String u IO b -> ParsecT String u IO b
-- buildKeywordParser k p = do
--   spaces
--   symbol <- p
--   guard $ symbol == k
--   pure symbol

-- notNewLine :: ParsecT String u IO ()
-- notNewLine = do
--   c <- anyChar
--   guard $ c /= '\n'
--   pure ()

-- asteriskLex :: ParsecT String u IO Char
-- asteriskLex = char '*'
