-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/03/28 02:27:02 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Parsec as P
import qualified Data.Set as S
import Control.Monad
-- import Control.Monad.IO.Class
import Data.List
import qualified Data.Map.Strict as M
-- import Control.Monad.State

type CProgramAST = [FunctionDefine]

data FunctionDefine =
  FunctionDefine
  {
    returnType :: String,
    funName :: String,
    inputArgs :: [InputArgPair],
    body :: [Statment]
  }
  deriving (Show, Eq)

data InputArgPair =
  ArgPair 
  {
    dataType :: String,
    varName :: String
  }
  deriving (Show, Eq)

data Statment =
  VariableDecl
  | Return Expr
  deriving (Show, Eq)

data Expr =
  Constant String
  | Variable String
  | FunctionCall
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  deriving (Show, Eq)

data UnaryOp =
  Complement
  | Negate
  deriving (Show, Eq)

data BinaryOp =
  Plus
  | Minus
  | Multiply
  | Division
  | Modulo
  deriving (Show, Eq)

type IRProgramAST = [IRFunctionDefine]

data IRFunctionDefine =
  IRFunctionDefine
  {
    irFuncName :: String,
    irInstruction :: [IRInstruction]
  }
  deriving (Show, Eq)

data IRInstruction =
  IRReturn IRVal
  | IRUnary
    {
      irOp :: UnaryOp,
      irSrc :: IRVal,
      irDst :: IRVal
    }
  deriving (Show, Eq)

data IRVal =
  IRConstant String
  | IRVar String
  deriving (Show, Eq)

type AsmProgramAST = [AsmFunctionDefine]

data AsmFunctionDefine =
  AsmFunctionDefine
  {
    asmFuncName :: String,
    instructions :: [AsmInstruction]
  }
  deriving (Show, Eq)

data AsmInstruction =
  Mov {src :: Operand, dst :: Operand}
  | Ret
  | AsmUnary AsmUnaryOp Operand
  | AsmBinary AsmBinaryOp Operand Operand
  | AllocateStack Int
  deriving (Show, Eq)

data Operand =
  Imm Int
  | Register Reg
  | Pseudo {identifier :: String}
  | Stack Int
  deriving Eq

data AsmUnaryOp =
  AsmNeg
  | AsmNot
  deriving Eq

data AsmBinaryOp =
  AsmPlus
  | AsmMius
  | AsmMul
  | AsmDiv
  | AsmMod
  deriving (Show, Eq)

data Reg =
  AX
  | R10
  | R10D
  deriving Eq

instance Show AsmUnaryOp where
  show AsmNeg = "negl"
  show AsmNot = "notl"

instance Show Operand where
  show (Imm i) = "$" ++ show i
  show (Register s) = "%" ++ show s
  show (Pseudo ident) = "tmpVar." ++ show ident
  show (Stack i) = show i ++ "(%rbp)"

instance Show Reg where
  show AX = "eax"
  show R10 = "r10"
  show R10D = "r10d"

allBinaryOp :: String
allBinaryOp = "+-*/%"

binaryOpPrecedence :: M.Map Char Int
binaryOpPrecedence = M.fromList $ zip allBinaryOp [4, 4, 5, 5, 5]

createSkipSpacesCharParser :: Char -> ParsecT String u IO Char
createSkipSpacesCharParser = (spaces >>) . char

ucLex :: ParsecT String u IO Char
ucLex = createSkipSpacesCharParser '_'

minusLex :: ParsecT String u IO String
minusLex = createSkipSpacesStringParser "-"

divLex :: ParsecT String u IO String
divLex = createSkipSpacesStringParser "/"

plusLex :: ParsecT String u IO String
plusLex = createSkipSpacesStringParser "+"

mulLex :: ParsecT String u IO String
mulLex = createSkipSpacesStringParser "*"

percentLex :: ParsecT String u IO String
percentLex = createSkipSpacesStringParser "%"

complementLex :: ParsecT String u IO String
complementLex = createSkipSpacesStringParser "~"

decrementLex :: ParsecT String u IO String
decrementLex = createSkipSpacesStringParser "--"

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

identifierParser :: ParsecT String u IO String
identifierParser = spaces >> symbolExtract

intParser :: ParsecT String u IO String
intParser = spaces >> many1 digit

fileParser :: (Ord u, Monoid u) => ParsecT String (S.Set u, Int) IO CProgramAST
fileParser = manyTill functionDefineParser (try (spaces >> eof))

-- oldexprParser :: ParsecT String u IO Expr
-- oldexprParser = do
--   op <- try openPParser
--     <|> try (minusLex <* notFollowedBy minusLex)
--     <|> try complementLex
--     <|> pure []
--   -- _ <- liftIO $ print op
--   expr <- case op of
--     "-" -> Unary Negate <$> oldexprParser
--     "~" -> Unary Complement <$> oldexprParser
--     "(" -> oldexprParser
--     _ -> Constant <$> intParser
--   void $ case op of
--     "(" -> closePParser
--     _ -> pure []
--   pure expr

binaryOpParser :: ParsecT String u IO BinaryOp
binaryOpParser = try (plusLex >> pure Plus)
   <|> try ((minusLex <* notFollowedBy minusLex) >> pure Minus)
   <|> try (mulLex >> pure Multiply)
   <|> try (divLex >> pure Division)
   <|> try (percentLex >> pure Modulo)

binaryExprParser :: ParsecT String (ds, Int) IO Expr
binaryExprParser = flip Binary
  <$> factorParser
  <*> binaryOpParser
  <*> factorParser

exprParser :: ParsecT String (ds, Int) IO Expr
exprParser = factorParser >>= exprRightParser

isBinaryOpChar :: Char -> Bool
isBinaryOpChar = flip elem allBinaryOp

updatePrecedence :: Num b => b -> (a, c) -> (a, b)
updatePrecedence p = (, p + 1) . fst

revokePrecedence :: b -> (a, c) -> (a, b)
revokePrecedence p = (, p) . fst

isEqOrHigherPrecedence :: Char -> Int -> Bool
isEqOrHigherPrecedence binOp p = (binaryOpPrecedence M.! binOp) >= p

exprRightParser :: Expr -> ParsecT String (ds, Int) IO Expr
exprRightParser lExpr = do
  binOp <- lookAhead $ spaces >> anyChar
  (_, p) <- getState
  if isBinaryOpChar binOp && isEqOrHigherPrecedence binOp p
    then
      do
        op <- binaryOpParser
        modifyState $ updatePrecedence $ binaryOpPrecedence M.! binOp
        rExpr <- factorParser
        modifyState $ revokePrecedence p
        exprRightParser $ Binary op lExpr rExpr
    else
      pure lExpr

negateOpParser :: ParsecT String (ds, Int) IO Expr
negateOpParser = do
  void (minusLex <* notFollowedBy (char '-'))
  Unary Negate <$> exprParser

complementOpParser :: ParsecT String (ds, Int) IO Expr
complementOpParser = do
  void complementLex
  Unary Complement <$> exprParser

intOperandParser :: ParsecT String u IO Expr
intOperandParser = Constant <$> intParser

parenExprParser :: ParsecT String (ds, Int) IO Expr
parenExprParser = do
  void openPParser
  expr <- exprParser
  void closePParser
  pure expr

factorParser :: ParsecT String (ds, Int) IO Expr
factorParser = try intOperandParser
  <|> try negateOpParser
  <|> try complementOpParser
  <|> try parenExprParser

returnStatParser :: ParsecT String (ds, Int) IO Statment
returnStatParser = do
  void keyReturnParser
  expr <- exprParser
  void semiColParser
  pure $ Return expr

argPairParser :: ParsecT String u IO InputArgPair
argPairParser = ArgPair <$> identifierParser <*> identifierParser

argListParser :: ParsecT String u IO [InputArgPair]
argListParser = do
  res <- try keyVoidParser <|> pure []
  case res of
    "void" -> pure []
    _ -> try (sepBy argPairParser (try commaParser)) <|> pure []

functionDefineParser :: (Ord u, Monoid u) => ParsecT String (S.Set u, Int) IO FunctionDefine
functionDefineParser = do
  -- modifyState $ S.insert mempty
  -- retType <- keyIntParser <|> keyVoidParser <|> identifierParser
  retType <- (keyIntParser <|> keyVoidParser) <* notFollowedBy (alphaNum <|> try ucLex)
  fName <- identifierParser
  argList <- between openPParser closePParser $ try argListParser
  void openCurParser
  statments <- manyTill returnStatParser (try closeCurParser)
  pure $ FunctionDefine retType fName argList statments

makeVarName :: Show a => a -> String
makeVarName i = "tmp." ++ show i

exprToIRList :: Int -> Expr -> [IRInstruction] -> (Int, [IRInstruction], IRVal)
exprToIRList i expr irs = case expr of
  Constant s -> (i, irs, IRConstant s)
  Unary op uexpr ->
    let (newi, oldIRList, irVal) = exprToIRList i uexpr irs in
      (newi + 1,
        oldIRList ++ [IRUnary op irVal (IRVar $ show newi)],
        IRVar $ show newi)
  _ -> undefined
  
addReturnToIRList :: (a, [IRInstruction], IRVal) -> [IRInstruction]
addReturnToIRList (_, irs, irVal) = irs ++ [IRReturn irVal]

cReturnStatmentToIRList :: Expr -> [IRInstruction]
cReturnStatmentToIRList expr = addReturnToIRList $ exprToIRList 1 expr []

cStatmentToIRInstructions :: Statment -> [IRInstruction]
cStatmentToIRInstructions (Return expr) = cReturnStatmentToIRList expr
cStatmentToIRInstructions _ = []

cFuncDefineToIRFuncDefine :: FunctionDefine -> IRFunctionDefine
cFuncDefineToIRFuncDefine fd =
  IRFunctionDefine (funName fd) (concatMap cStatmentToIRInstructions $ body fd)

cASTToIrAST :: CProgramAST -> IRProgramAST
cASTToIrAST = map cFuncDefineToIRFuncDefine

irASTToAsmAST :: IRProgramAST -> AsmProgramAST
irASTToAsmAST = map irFuncDefineToAsmFuncDefine

irFuncDefineToAsmFuncDefine :: IRFunctionDefine -> AsmFunctionDefine
irFuncDefineToAsmFuncDefine fd =
  AsmFunctionDefine
    (irFuncName fd)
    (concatMap
      irInstructionToAsmInstruction
      (irInstruction fd))

irOperandToAsmOperand :: IRVal -> Operand
irOperandToAsmOperand (IRConstant i) = Imm $ read i
irOperandToAsmOperand (IRVar s) = Pseudo s

irOpToAsmOp :: UnaryOp -> AsmUnaryOp
irOpToAsmOp Complement = AsmNot
irOpToAsmOp Negate = AsmNeg

irInstructionToAsmInstruction :: IRInstruction -> [AsmInstruction]
irInstructionToAsmInstruction (IRReturn val) =
  [Mov (irOperandToAsmOperand val) (Register AX),
    Ret]
irInstructionToAsmInstruction (IRUnary op s d) =
  [Mov (irOperandToAsmOperand s) (irOperandToAsmOperand d),
    AsmUnary (irOpToAsmOp op) (irOperandToAsmOperand d)]

noExecutableStackString :: String
noExecutableStackString =  tabulate [".section", ".note.GNU-stack,\"\",@progbits"] ++ "\n"

convertAsmTempVarToStackAddr :: [AsmInstruction] -> [AsmInstruction]
convertAsmTempVarToStackAddr = map convertInstr
  where convertInstr instr =
          case instr of
            Mov s d -> Mov (convertOperand s) (convertOperand d)
            AsmUnary op d -> AsmUnary op $ convertOperand d
            _ -> instr
        convertOperand operand = 
          case operand of
            Pseudo ident -> Stack ((-4) * read ident)
            _ -> operand

addAllocateStackToFunc :: [AsmInstruction] -> [AsmInstruction]
addAllocateStackToFunc instrs = AllocateStack ((-1) * getStackSize instrs) : instrs

getStackSize :: [AsmInstruction] -> Int
getStackSize = foldl getMinSize 0
  where getMinSize x y = min x $ takeMaxValFromInstr y
        takeMaxValFromInstr instr =
          case instr of
            Mov s d -> max (takeValFromOperand s) (takeValFromOperand d)
            AsmUnary _ d -> takeValFromOperand d
            _ -> 0
        takeValFromOperand operand =
          case operand of
            Stack i -> i
            _ -> 0

replacePseudoRegAllocateStackFixDoubleStackOperand :: AsmFunctionDefine -> AsmFunctionDefine
replacePseudoRegAllocateStackFixDoubleStackOperand afd =
  afd {instructions = concatMap resolveDoubleStackOperand
                    $ addAllocateStackToFunc
                    $ convertAsmTempVarToStackAddr
                    $ instructions afd}

resolveDoubleStackOperand :: AsmInstruction -> [AsmInstruction]
resolveDoubleStackOperand instr =
  case instr of
    Mov (Stack i) (Stack j) ->
      [Mov (Stack i) (Register R10D),
        Mov (Register R10D) (Stack j)]
    _ -> [instr]

asmProgramASTToAsm :: [AsmFunctionDefine] -> String
asmProgramASTToAsm = unlines . map asmFunctionDefineToStr

tabulate :: [String] -> String
tabulate = intercalate "\t" . ("" :)

asmFuncReturnStr :: [String]
asmFuncReturnStr = 
  [tabulate ["movq", "%rbp, %rsp"],
  tabulate ["popq", "%rbp"],
  tabulate ["ret"]]

asmInstructionToStr :: AsmInstruction -> [String]
asmInstructionToStr Ret = asmFuncReturnStr
asmInstructionToStr (Mov s d) = pure $ tabulate ["movl", show s ++ ", " ++ show d]
asmInstructionToStr (AsmUnary op d) = pure $ tabulate [show op, show d]
asmInstructionToStr (AllocateStack i) = 
  case i of
    0 -> []
    _ -> pure $ tabulate ["subq", "$" ++ show i ++ ", %rsp"]
asmInstructionToStr _ = undefined

asmFunctionDefineToStr :: AsmFunctionDefine -> String
asmFunctionDefineToStr (AsmFunctionDefine fname instrs) =
  unlines [tabulate [".globl", fname],
    fname ++ ":",
    tabulate ["pushq", "%rbp"],
    tabulate ["movq", "%rsp, %rbp"],
    unlines $ concatMap asmInstructionToStr instrs]
