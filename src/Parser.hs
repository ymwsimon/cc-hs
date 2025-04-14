-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/04/14 15:41:45 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Parsec as P
import qualified Data.Set as S
import Control.Monad
-- import Control.Monad.IO.Class
import Operation
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

lowestPrecedence :: Int
lowestPrecedence = 16

allBinaryOp :: [String]
allBinaryOp =
 ["+", "-", "*", "/",
 "%", "&&", "||", "&",
 "|", ">>", "<<", "^",
 "==", "!=", "<", ">",
 "<=", ">=", "!"]

binaryOpPrecedence :: M.Map String Int
binaryOpPrecedence = M.fromList $ zip allBinaryOp
  [4, 4, 3, 3,
  3, 11, 12, 8,
  10, 5, 5, 9,
  7, 7, 6, 6,
  6, 6, 2]

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

bitAndLex :: ParsecT String u IO String
bitAndLex = createSkipSpacesStringParser "&"

bitOrLex :: ParsecT String u IO String
bitOrLex = createSkipSpacesStringParser "|"

bitXorLex :: ParsecT String u IO String
bitXorLex = createSkipSpacesStringParser "^"

bitShiftLeftLex :: ParsecT String u IO String
bitShiftLeftLex = createSkipSpacesStringParser "<<"

bitShiftRightLex :: ParsecT String u IO String
bitShiftRightLex = createSkipSpacesStringParser ">>"

exclaimLex :: ParsecT String u IO String
exclaimLex = createSkipSpacesStringParser "!"

logicAndLex :: ParsecT String u IO String
logicAndLex = createSkipSpacesStringParser "&&"

logicOrLex :: ParsecT String u IO String
logicOrLex = createSkipSpacesStringParser "||"

equalRelationLex :: ParsecT String u IO String
equalRelationLex = createSkipSpacesStringParser "=="

notEqualRelationLex :: ParsecT String u IO String
notEqualRelationLex = createSkipSpacesStringParser "!="

lessThanRelationLex :: ParsecT String u IO String
lessThanRelationLex = createSkipSpacesStringParser "<"

greatThanRelationLex :: ParsecT String u IO String
greatThanRelationLex = createSkipSpacesStringParser ">"

lessEqualThanRelationLex :: ParsecT String u IO String
lessEqualThanRelationLex = createSkipSpacesStringParser "<="

greatEqualThanRelationLex :: ParsecT String u IO String
greatEqualThanRelationLex = createSkipSpacesStringParser ">="

decrementLex :: ParsecT String u IO String
decrementLex = createSkipSpacesStringParser "--"

symbolExtract :: ParsecT String u IO String
symbolExtract = do
  firstLetter <- letter <|> try ucLex
  tailLetters <- many $ alphaNum <|> try ucLex
  pure $ firstLetter : tailLetters

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
fileParser = manyTill functionDefineParser $ try $ spaces >> eof

binaryOpParser :: ParsecT String u IO BinaryOp
binaryOpParser = foldl1 (<|>) $
  map try $
    zipWith (>>)
      [
        bitAndLex <* notFollowedBy (char '&'),
        bitOrLex <* notFollowedBy (char '|'),
        bitXorLex,
        bitShiftLeftLex,
        bitShiftRightLex,
        plusLex <* notFollowedBy (char '+'),
        minusLex <* notFollowedBy (char '-'),
        mulLex,
        divLex,
        percentLex,
        logicAndLex,
        logicOrLex,
        equalRelationLex,
        notEqualRelationLex,
        lessEqualThanRelationLex,
        lessThanRelationLex,
        greatEqualThanRelationLex,
        greatThanRelationLex
      ] $
      map pure
        [
          BitAnd,
          BitOr,
          BitXor,
          BitShiftLeft,
          BitShiftRight,
          Plus,
          Minus,
          Multiply,
          Division,
          Modulo,
          LogicAnd,
          LogicOr,
          EqualRelation,
          NotEqualRelation,
          LessEqualRelation,
          LessThanRelation,
          GreaterEqualRelation,
          GreaterThanRelation
        ]

binaryOpStringParser :: ParsecT String u IO String
binaryOpStringParser = foldl1 (<|>) $
  map try
    [bitAndLex <* notFollowedBy (char '&'),
    bitOrLex <* notFollowedBy (char '|'),
    bitXorLex,
    bitShiftLeftLex,
    bitShiftRightLex,
    plusLex <* notFollowedBy (char '+'),
    minusLex <* notFollowedBy (char '-'),
    mulLex,
    divLex,
    percentLex,
    logicAndLex,
    logicOrLex,
    equalRelationLex,
    notEqualRelationLex,
    lessThanRelationLex,
    lessEqualThanRelationLex,
    greatThanRelationLex,
    greatEqualThanRelationLex]

binaryExprParser :: ParsecT String (ds, Int) IO Expr
binaryExprParser = flip Binary
  <$> factorParser
  <*> binaryOpParser
  <*> factorParser

exprParser :: ParsecT String (ds, Int) IO Expr
exprParser = factorParser >>= exprRightParser

isBinaryOpChar :: String -> Bool
isBinaryOpChar = flip elem allBinaryOp

getPrecedence :: String -> Int
getPrecedence = (binaryOpPrecedence M.!)

updatePrecedence :: Num b => b -> (a, c) -> (a, b)
updatePrecedence p = (, p - 1) . fst

revokePrecedence :: b -> (a, c) -> (a, b)
revokePrecedence p = (, p) . fst

isEqOrHigherPrecedence :: String -> Int -> Bool
isEqOrHigherPrecedence binOp p = (binaryOpPrecedence M.! binOp) <= p

exprRightParser :: Expr -> ParsecT String (ds, Int) IO Expr
exprRightParser lExpr = do
  binOp <- lookAhead binaryOpStringParser <|> pure []
  p <- snd <$> getState
  if isBinaryOpChar binOp && isEqOrHigherPrecedence binOp p
    then
      do
        op <- binaryOpParser
        modifyState $ updatePrecedence $ getPrecedence binOp
        rExpr <- exprParser
        modifyState $ revokePrecedence p
        exprRightParser $ Binary op lExpr rExpr
    else
      pure lExpr

unaryOpParser :: ParsecT String (ds, Int) IO Expr
unaryOpParser = try negateOpParser
  <|> try complementOpParser
  <|> try notRelationOpParser

negateOpParser :: ParsecT String (ds, Int) IO Expr
negateOpParser = do
  p <- snd <$> getState
  void $ minusLex <* notFollowedBy (char '-')
  modifyState $ updatePrecedence 2
  (Unary Negate <$> exprParser) <* modifyState (revokePrecedence p)

complementOpParser :: ParsecT String (ds, Int) IO Expr
complementOpParser = do
  p <- snd <$> getState
  void complementLex
  modifyState $ updatePrecedence 2
  (Unary Complement <$> exprParser) <* modifyState (revokePrecedence p)

notRelationOpParser :: ParsecT String (ds, Int) IO Expr
notRelationOpParser = do
  p <- snd <$> getState
  void exclaimLex
  modifyState $ updatePrecedence 2
  (Unary NotRelation <$> exprParser) <* modifyState (revokePrecedence p)

intOperandParser :: ParsecT String u IO Expr
intOperandParser = Constant <$> intParser

parenExprParser :: ParsecT String (ds, Int) IO Expr
parenExprParser = do
  void openPParser
  p <- snd <$> getState
  modifyState $ updatePrecedence lowestPrecedence
  expr <- exprParser
  void closePParser
  modifyState $ revokePrecedence p
  pure expr

factorParser :: ParsecT String (ds, Int) IO Expr
factorParser = try intOperandParser
  <|> try unaryOpParser
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
    _ -> try (sepBy argPairParser $ try commaParser) <|> pure []

functionDefineParser :: (Ord u, Monoid u) => ParsecT String (S.Set u, Int) IO FunctionDefine
functionDefineParser = do
  -- retType <- keyIntParser <|> keyVoidParser <|> identifierParser
  retType <- (keyIntParser <|> keyVoidParser) <* notFollowedBy (alphaNum <|> try ucLex)
  fName <- identifierParser
  argList <- between openPParser closePParser $ try argListParser
  void openCurParser
  statments <- manyTill returnStatParser $ try closeCurParser
  pure $ FunctionDefine retType fName argList statments
