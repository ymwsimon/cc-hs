-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/06/19 18:03:59 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

{-# LANGUAGE TupleSections #-}

module Parser where

import Text.Parsec as P
import Control.Monad
import Control.Monad.IO.Class
import Operation
import qualified Data.Map.Strict as M
import Data.Char (isLetter, isAlphaNum, isDigit)
import Data.Either (isLeft, fromRight, fromLeft)

type CProgramAST = [FunctionDefine]

type VarType = String

type IdentifierName = String

data JumpLabel =
  LoopLabel (String, String, String)
  | SwitchLabel (String, [(Int, String)])
  | NoLabel
  deriving (Show, Eq)

data ParseInfo = 
  ParseInfo
  {
    currentScopeVar :: M.Map String String,
    outerScopeVar :: M.Map String String,
    precedence :: Int,
    labelId :: Int,
    jumpLabel :: JumpLabel
  }

data FunctionDefine =
  FunctionDefine
  {
    returnType :: String,
    funName :: String,
    inputArgs :: [InputArgPair],
    body :: Block,
    nextVarId :: Int
  }
  deriving (Show, Eq)

data InputArgPair =
  ArgPair 
  {
    dataType :: String,
    varName :: String
  }
  deriving (Show, Eq)

data ForInit =
  InitDecl Declaration
  | InitExpr (Maybe Expr)
  deriving (Show, Eq)

data Statement =
  Expression Expr
  | Return Expr
  | If Expr Statement (Maybe Statement)
  | Goto String
  | Label String Statement
  | Compound Block
  | Break String
  | Continue String
  | While Expr Statement JumpLabel
  | DoWhile Statement Expr JumpLabel
  | For ForInit (Maybe Expr) (Maybe Expr) Statement JumpLabel
  | Null
  deriving (Show, Eq)

data Declaration =
  VariableDecl VarType IdentifierName (Maybe Expr)
  deriving (Show, Eq)

data BlockItem =
  S Statement
  | D Declaration
  deriving (Show, Eq)

newtype Block = Block {unBlock :: [BlockItem]}
  deriving (Show, Eq)

data Expr =
  Constant String
  | Variable String
  | FunctionCall
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Assignment BinaryOp Expr Expr
  | Conditional Expr Expr Expr
  deriving (Show, Eq)

lowestPrecedence :: Int
lowestPrecedence = 16

allBinaryOp :: [String]
allBinaryOp =
 ["+", "-", "*", "/",
 "%", "&&", "||", "&",
 "|", ">>", "<<", "^",
 "==", "!=", "<", ">",
 "<=", ">=", "!", "=",
 "+=", "-=", "*=", "/=",
 "%=", "&=", "|=", "^=",
 "<<=", ">>=", "?"]

allUnaryOp :: [String]
allUnaryOp =
  ["!", "~", "--", "++", "-", "+"]

allPostUnaryOp :: [String]
allPostUnaryOp =
  ["++", "--"]

binaryAssignmentOp :: [String]
binaryAssignmentOp =
  ["=", "+=", "-=", "*=", "/=",
  "%=", "&=", "|=", "^=", "<<=", ">>="]

keywordList :: [String]
keywordList = 
  ["int", "void", "return", "if", "else", "goto", "do", "while", "for", "break", "continue"]

binaryOpPrecedence :: M.Map String Int
binaryOpPrecedence = M.fromList $ zip allBinaryOp
  [4, 4, 3, 3,
  3, 11, 12, 8,
  10, 5, 5, 9,
  7, 7, 6, 6,
  6, 6, 2, 14,
  14, 14, 14, 14,
  14, 14, 14, 14,
  14, 14, 13]

unaryOpPrecedence :: M.Map String Int
unaryOpPrecedence = M.fromList $ map (, 2) allUnaryOp

postUnaryOpPrecedence :: M.Map String Int
postUnaryOpPrecedence = M.fromList $ map (, 1) allPostUnaryOp

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

equalLex :: ParsecT String u IO String
equalLex = createSkipSpacesStringParser "="

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

assignmentLex :: ParsecT String u IO String
assignmentLex = createSkipSpacesStringParser "="

plusAssignLex :: ParsecT String u IO String
plusAssignLex = createSkipSpacesStringParser "+="

minusAssignLex :: ParsecT String u IO String
minusAssignLex = createSkipSpacesStringParser "-="

multiAssignLex :: ParsecT String u IO String
multiAssignLex = createSkipSpacesStringParser "*="

divAssignLex :: ParsecT String u IO String
divAssignLex = createSkipSpacesStringParser "/="

modAssignLex :: ParsecT String u IO String
modAssignLex = createSkipSpacesStringParser "%="

bitAndAssignLex :: ParsecT String u IO String
bitAndAssignLex = createSkipSpacesStringParser "&="

bitOrAssignLex :: ParsecT String u IO String
bitOrAssignLex = createSkipSpacesStringParser "|="

bitXorAssignLex :: ParsecT String u IO String
bitXorAssignLex = createSkipSpacesStringParser "^="

bitLeftShiftAssignLex :: ParsecT String u IO String
bitLeftShiftAssignLex = createSkipSpacesStringParser "<<="

bitRightShiftAssignLex :: ParsecT String u IO String
bitRightShiftAssignLex = createSkipSpacesStringParser ">>="

incrementLex :: ParsecT String u IO String
incrementLex = createSkipSpacesStringParser "++"

decrementLex :: ParsecT String u IO String
decrementLex = createSkipSpacesStringParser "--"

symbolExtract :: ParsecT String u IO String
symbolExtract = do
  spaces
  firstLetter <- satisfy (\c -> (c == '_') || isLetter c)
  tailLetters <- many (satisfy (\c -> (c == '_') || isAlphaNum c))
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

questionParser :: ParsecT String u IO String
questionParser = createSkipSpacesStringParser "?"

colonParser :: ParsecT String u IO String
colonParser = createSkipSpacesStringParser ":"

keywordParserCreate :: String -> ParsecT String u IO String
keywordParserCreate k = do
  spaces
  symbol <- symbolExtract
  if symbol == k
    then pure k
    else unexpected $ "Not equal the keyword " ++ k ++ ": " ++ symbol
  
keyIntParser :: ParsecT String u IO String
keyIntParser = keywordParserCreate "int"

keyVoidParser :: ParsecT String u IO String
keyVoidParser = keywordParserCreate "void"

keyReturnParser :: ParsecT String u IO String
keyReturnParser = keywordParserCreate "return"

keyIfParser :: ParsecT String u IO String
keyIfParser = keywordParserCreate "if"

keyElseParser :: ParsecT String u IO String
keyElseParser = keywordParserCreate "else"

keyGotoParser :: ParsecT String u IO String
keyGotoParser = keywordParserCreate "goto"

keyBreakParser :: ParsecT String u IO String
keyBreakParser = keywordParserCreate "break"

keyContinueParser :: ParsecT String u IO String
keyContinueParser = keywordParserCreate "continue"

keyWhileParser :: ParsecT String u IO String
keyWhileParser = keywordParserCreate "while"

keyDoParser :: ParsecT String u IO String
keyDoParser = keywordParserCreate "do"

keyForParser :: ParsecT String u IO String
keyForParser = keywordParserCreate "for"

identifierParser :: ParsecT String u IO String
identifierParser = do
  symbol <- symbolExtract
  if symbol `elem` keywordList
    then unexpected $ "Cannot use keyword as identifier: " ++ symbol
    else pure symbol

intParser :: ParsecT String u IO String
intParser = spaces >> many1 digit

fileParser :: ParsecT String ParseInfo IO [FunctionDefine]
fileParser = manyTill functionDefineParser $ try $ spaces >> eof

binaryAssignmentOpParser :: ParsecT String u IO BinaryOp
binaryAssignmentOpParser = foldl1 (<|>) $
  map try $
    zipWith (>>)
      [plusAssignLex, minusAssignLex, multiAssignLex, divAssignLex,
        modAssignLex, bitAndAssignLex, bitOrAssignLex, bitXorAssignLex,
        bitLeftShiftAssignLex, bitRightShiftAssignLex, assignmentLex] $
      map pure
        [Plus, Minus, Multiply, Division,
          Modulo, BitAnd, BitOr, BitXor,
          BitShiftLeft, BitShiftRight, None]

binaryOpParser :: ParsecT String u IO BinaryOp
binaryOpParser = foldl1 (<|>) $
  map try $
    zipWith (>>)
      [bitAndLex <* notFollowedBy (char '&'), bitOrLex <* notFollowedBy (char '|'),
        bitXorLex, bitShiftLeftLex, bitShiftRightLex, plusLex <* notFollowedBy (char '+'),
        minusLex <* notFollowedBy (char '-'), mulLex, divLex, percentLex,
        logicAndLex, logicOrLex, equalRelationLex, notEqualRelationLex,
        lessEqualThanRelationLex, lessThanRelationLex,
        greatEqualThanRelationLex, greatThanRelationLex] $
      map pure
        [BitAnd, BitOr,
          BitXor, BitShiftLeft, BitShiftRight, Plus,
          Minus, Multiply, Division, Modulo,
          LogicAnd, LogicOr, EqualRelation, NotEqualRelation,
          LessEqualRelation, LessThanRelation,
          GreaterEqualRelation, GreaterThanRelation]

unaryOpParser :: ParsecT String u IO UnaryOp
unaryOpParser = foldl1 (<|>) $
  map try $
    zipWith (>>)
      [incrementLex, decrementLex, plusLex,
        minusLex, exclaimLex, complementLex] $
      map pure
        [PreIncrement, PreDecrement, UPlus,
          Negate, NotRelation, Complement]

postUnaryOpParser :: ParsecT String u IO UnaryOp
postUnaryOpParser = foldl1 (<|>) $
  map try $
    zipWith (>>) [incrementLex, decrementLex] $
      map pure [PostIncrement, PostDecrement]

binaryOpStringParser :: ParsecT String u IO String
binaryOpStringParser = foldl1 (<|>) $
  map try
    [plusAssignLex, minusAssignLex, multiAssignLex, divAssignLex,
      modAssignLex, bitAndAssignLex, bitOrAssignLex, bitXorAssignLex,
      bitLeftShiftAssignLex, bitRightShiftAssignLex,
      bitAndLex <* notFollowedBy (char '&'), bitOrLex <* notFollowedBy (char '|'),
      bitXorLex, bitShiftLeftLex, bitShiftRightLex, plusLex <* notFollowedBy (char '+'),
      minusLex <* notFollowedBy (char '-'), mulLex, divLex, percentLex,
      logicAndLex, logicOrLex, equalLex <* notFollowedBy (char '='),
      equalRelationLex, notEqualRelationLex,
      lessThanRelationLex, lessEqualThanRelationLex,
      greatThanRelationLex, greatEqualThanRelationLex,
      questionParser]

unaryOpStringParser :: ParsecT String u IO String
unaryOpStringParser = foldl1 (<|>) $
  map try
    [incrementLex, decrementLex, plusLex, minusLex,
      exclaimLex, complementLex]

binaryExprParser :: ParsecT String ParseInfo IO Expr
binaryExprParser = flip Binary
  <$> factorParser
  <*> binaryOpParser
  <*> factorParser

unaryExprParser :: ParsecT String ParseInfo IO Expr
unaryExprParser = do
  uOpStr <- lookAhead unaryOpStringParser
  uOp <- unaryOpParser
  p <- precedence <$> getState
  modifyState $ updatePrecedence $ getUnaryOpPrecedence uOpStr
  expr <- exprParser
  if uOp `elem` [PreDecrement, PreIncrement]
    then case expr of
      Variable _ -> Unary uOp expr <$ modifyState (setPrecedence p)
      _ -> unexpected "Need lvalue for prefix operation"
    else Unary uOp expr <$ modifyState (setPrecedence p)

exprParser :: ParsecT String ParseInfo IO Expr
exprParser = factorParser >>= exprRightParser

isBinaryOpChar :: String -> Bool
isBinaryOpChar = flip elem allBinaryOp

getBinOpPrecedence :: String -> Int
getBinOpPrecedence = (binaryOpPrecedence M.!)

getUnaryOpPrecedence :: String -> Int
getUnaryOpPrecedence = (unaryOpPrecedence M.!)

updatePrecedence :: Int -> ParseInfo -> ParseInfo
updatePrecedence p parseInfo = parseInfo {precedence = p - 1}

setPrecedence :: Int -> ParseInfo -> ParseInfo
setPrecedence p parseInfo = parseInfo {precedence = p}

updateCurrentScopeVar :: M.Map String String -> ParseInfo -> ParseInfo
updateCurrentScopeVar m parseInfo = parseInfo {currentScopeVar = m}

updateOuterScopeVar :: M.Map String String -> ParseInfo -> ParseInfo
updateOuterScopeVar m parseInfo = parseInfo {outerScopeVar = m}

bumpLabelId :: ParsecT s ParseInfo IO ()
bumpLabelId = modifyState (\parseInfo -> parseInfo {labelId = labelId parseInfo + 1})

updateJumpLabel :: JumpLabel -> ParseInfo -> ParseInfo
updateJumpLabel jl parseInfo = parseInfo {jumpLabel = jl}

getCurrentScopeVar :: ParsecT s ParseInfo IO (M.Map String String)
getCurrentScopeVar = currentScopeVar <$> getState

getOuterScopeVar :: ParsecT s ParseInfo IO (M.Map String String)
getOuterScopeVar = outerScopeVar <$> getState

getPrecedence :: ParsecT s ParseInfo IO Int
getPrecedence = precedence <$> getState

getNextLabelId :: ParsecT s ParseInfo IO Int
getNextLabelId = labelId <$> getState

getJumpLabel :: ParsecT s ParseInfo IO JumpLabel
getJumpLabel = jumpLabel <$> getState

useNewLabelId :: ParseInfo -> ParsecT s ParseInfo IO ()
useNewLabelId oldParseIno = do
  lId <- labelId <$> getState
  putState $ oldParseIno {labelId = lId}

isEqOrHigherPrecedence :: String -> Int -> Bool
isEqOrHigherPrecedence binOp p = (binaryOpPrecedence M.! binOp) <= p

condtionalTrueParser :: ParsecT String ParseInfo IO Expr
condtionalTrueParser = do
  void questionParser
  oldState <- precedence <$> getState
  modifyState $ updatePrecedence lowestPrecedence
  expr <- exprParser
  modifyState $ setPrecedence oldState
  void colonParser
  pure expr

exprRightParser :: Expr -> ParsecT String ParseInfo IO Expr
exprRightParser lExpr = do
  binOp <- lookAhead (try binaryOpStringParser) <|> pure ""
  p <- precedence <$> getState
  if isBinaryOpChar binOp && isEqOrHigherPrecedence binOp p
    then if binOp `elem` binaryAssignmentOp
          then case lExpr of
                Variable _ -> do
                    op <- binaryAssignmentOpParser
                    modifyState $ setPrecedence $ getBinOpPrecedence binOp
                    rExpr <- exprParser
                    exprRightParser $ Assignment op lExpr rExpr
                _ -> unexpected "Invalid lvalue on the left side"
          else if binOp == "?"
            then do
              tCond <- condtionalTrueParser
              modifyState $ setPrecedence $ getBinOpPrecedence binOp
              fCond <- exprParser
              exprRightParser $ Conditional lExpr tCond fCond
            else do
              op <- binaryOpParser
              modifyState $ updatePrecedence $ getBinOpPrecedence binOp
              rExpr <- exprParser
              modifyState $ setPrecedence p
              exprRightParser $ Binary op lExpr rExpr
    else pure lExpr

intOperandParser :: ParsecT String u IO Expr
intOperandParser = Constant <$> intParser

postfixOp :: Expr -> ParsecT String u IO Expr
postfixOp expr = case expr of
    Variable _ -> do
      maybePostOp <- lookAhead (try $ incrementLex <|> decrementLex) <|> pure ""
      if maybePostOp `elem` allPostUnaryOp
                    then ($ expr) . Unary <$> postUnaryOpParser
                    else pure expr
    _ -> pure expr

parenExprParser :: ParsecT String ParseInfo IO Expr
parenExprParser = do
  void openPParser
  p <- precedence <$> getState
  modifyState $ updatePrecedence lowestPrecedence
  expr <- exprParser
  void closePParser
  modifyState $ setPrecedence p
  postfixOp expr

variableParser :: ParsecT String ParseInfo IO Expr
variableParser = do
  vName <- identifierParser
  current <- currentScopeVar <$> getState
  outer <- outerScopeVar <$> getState
  if not (M.member vName current) && not (M.member vName outer)
    then unexpected $ "Undefined variable: " ++ vName
    else if M.member vName current
      then postfixOp $ Variable (current M.! vName)
      else postfixOp $ Variable (outer M.! vName)

factorParser :: ParsecT String ParseInfo IO Expr
factorParser = do
  spaces
  c <- lookAhead anyChar
  next c
    where next c
            | isDigit c = intOperandParser
            | [c] `elem` allUnaryOp = unaryExprParser
            | c == '(' = parenExprParser
            | otherwise = variableParser

returnStatParser :: ParsecT String ParseInfo IO Statement
returnStatParser = do
  void keyReturnParser
  expr <- exprParser
  void semiColParser
  pure $ Return expr

ifStatParser :: ParsecT String ParseInfo IO Statement
ifStatParser = do
  void keyIfParser
  cond <- parenExprParser
  tStat <- statementParser
  maybeElse <- lookAhead $ try keyElseParser <|> pure ""
  fStat <- case maybeElse of
    "else" -> do
        void keyElseParser
        Just <$> statementParser
    _ -> pure Nothing
  pure $ If cond tStat fStat

argPairParser :: ParsecT String u IO InputArgPair
argPairParser = ArgPair <$> identifierParser <*> identifierParser

argListParser :: ParsecT String u IO [InputArgPair]
argListParser = do
  res <- try keyVoidParser <|> pure []
  case res of
    "void" -> pure []
    _ -> try (sepBy argPairParser $ try commaParser) <|> pure []

declarationParser :: ParsecT String ParseInfo IO Declaration
declarationParser = do
  varType <- keyIntParser
  vName <- identifierParser <?> "Valid identifier"
  parseInfo <- getState
  if M.member vName $ currentScopeVar parseInfo
    then unexpected $ "Variable redeclare: " ++ vName
    else do
      let varId = read $ currentScopeVar parseInfo M.! varIdMapKey
          newVarId = (+ (1 :: Int)) varId
          newVarName = vName ++ "#" ++ show varId
          newVarMap = M.adjust (const (show newVarId)) varIdMapKey $ currentScopeVar parseInfo
      putState $ parseInfo {currentScopeVar = M.insert vName newVarName newVarMap}
      maybeEqual <- optionMaybe $ try equalLex
      initialiser <-
        case maybeEqual of
          Just _ -> Just <$> exprParser
          _ -> pure Nothing
      void semiColParser
      pure $ VariableDecl varType newVarName initialiser

expressionParser :: ParsecT String ParseInfo IO Statement
expressionParser = Expression <$> exprParser

nullStatParser :: ParsecT String ParseInfo IO Statement
nullStatParser = semiColParser >> pure Null

gotoParser :: ParsecT String ParseInfo IO Statement
gotoParser = keyGotoParser >> Goto <$> symbolExtract <* semiColParser

labelParser :: ParsecT String ParseInfo IO Statement
labelParser = do
  lName <- identifierParser
  void colonParser
  stat <- statementParser <?> "Statement only"
  pure $ Label lName stat

labelNameParser :: ParsecT String u IO String
labelNameParser = do
  lName <- identifierParser
  void colonParser
  pure lName

blockParser :: ParsecT String ParseInfo IO Block
blockParser = do
  void openCurParser
  parseInfo <- getState
  putState $ parseInfo
    {currentScopeVar = M.singleton varIdMapKey $ currentScopeVar parseInfo M.! varIdMapKey,
      outerScopeVar = M.union (currentScopeVar parseInfo) (outerScopeVar parseInfo),
      precedence = lowestPrecedence}
  block <- manyTill blockItemParser $ try closeCurParser
  newVarId <- (M.! varIdMapKey) <$> getCurrentScopeVar
  putState $ parseInfo {currentScopeVar = M.adjust (const newVarId) varIdMapKey (currentScopeVar parseInfo)}
  pure $ Block block

breakParser :: ParsecT String ParseInfo IO Statement
breakParser = do
  l <- getJumpLabel
  case l of
    NoLabel -> unexpected "Break"
    LoopLabel (_, _, doneLabel) -> keyBreakParser >> Break doneLabel <$ semiColParser
    SwitchLabel _ -> keyBreakParser >> undefined

continueParser :: ParsecT String ParseInfo IO Statement
continueParser = do
  l <- getJumpLabel
  case l of
    NoLabel -> unexpected "Continue"
    LoopLabel (_, continueLabel, _) -> keyContinueParser >> Continue continueLabel <$ semiColParser
    SwitchLabel _ -> keyContinueParser >> undefined

makeLoopLabel :: ParsecT String ParseInfo IO ()
makeLoopLabel = do
  startLabel <- ("start" ++) . show <$> getNextLabelId <* bumpLabelId
  continueLabel <- ("continue" ++) . show <$> getNextLabelId <* bumpLabelId
  doneLabel <- ("done" ++) . show <$> getNextLabelId <* bumpLabelId
  modifyState $ updateJumpLabel $ LoopLabel (startLabel, continueLabel, doneLabel)

whileParser :: ParsecT String ParseInfo IO Statement
whileParser = do
  void keyWhileParser
  parseInfo <- getState
  makeLoopLabel
  jumpL <- getJumpLabel
  condition <- parenExprParser
  whileBody <- statementParser
  newVarId <- (M.! varIdMapKey) <$> getCurrentScopeVar
  lId <- labelId <$> getState
  putState $ parseInfo
    {currentScopeVar = M.adjust (const newVarId) varIdMapKey (currentScopeVar parseInfo),
      labelId = lId}
  pure $ While condition whileBody jumpL

doWhileParser :: ParsecT String ParseInfo IO Statement
doWhileParser = do
  void keyDoParser
  parseInfo <- getState
  makeLoopLabel
  jumpL <- getJumpLabel
  doWhileBody <- statementParser
  void keyWhileParser
  condition <- parenExprParser <* semiColParser
  newVarId <- (M.! varIdMapKey) <$> getCurrentScopeVar
  lId <- labelId <$> getState
  putState $ parseInfo
    {currentScopeVar = M.adjust (const newVarId) varIdMapKey (currentScopeVar parseInfo),
      labelId = lId}
  pure $ DoWhile doWhileBody condition jumpL

forInitParser :: ParsecT String ParseInfo IO ForInit
forInitParser = do
  maybeType <- lookAhead (try keyIntParser) <|> pure ""
  case maybeType of
    "int" -> InitDecl <$> declarationParser
    _ -> InitExpr <$> optionMaybe exprParser <* semiColParser

forLoopParser :: ParsecT String ParseInfo IO Statement
forLoopParser = do
  void $ keyForParser >> openPParser
  parseInfo <- getState
  putState $ parseInfo
    {currentScopeVar = M.singleton varIdMapKey $ currentScopeVar parseInfo M.! varIdMapKey,
      outerScopeVar = M.union (currentScopeVar parseInfo) (outerScopeVar parseInfo),
      precedence = lowestPrecedence}
  makeLoopLabel
  jumpL <- getJumpLabel
  forInit <- forInitParser
  condition <- optionMaybe (try exprParser) <* semiColParser
  postBody <- optionMaybe (try exprParser) <* closePParser
  forBody <- statementParser
  newVarId <- (M.! varIdMapKey) <$> getCurrentScopeVar
  lId <- labelId <$> getState
  putState $ parseInfo
    {currentScopeVar = M.adjust (const newVarId) varIdMapKey (currentScopeVar parseInfo),
      labelId = lId}
  pure $ For forInit condition postBody forBody jumpL

statementParser :: ParsecT String ParseInfo IO Statement
statementParser = do
    sym <- lookAhead (try symbolExtract <|> try openCurParser <|> try semiColParser) <|> pure ""
    case sym of 
      "return" -> returnStatParser
      "if" -> ifStatParser
      ";" -> nullStatParser
      "goto" -> gotoParser
      "{" -> Compound <$> blockParser
      "break" -> breakParser
      "continue" -> continueParser
      "while" -> whileParser
      "do" -> doWhileParser
      "for" -> forLoopParser
      _ -> do
        if sym `elem` keywordList
          then unexpected "Declaration"
          else do
              maybeLabel <- lookAhead (try labelNameParser) <|> pure ""
              if null maybeLabel
                then expressionParser <* semiColParser
                else labelParser

blockItemParser :: ParsecT String ParseInfo IO BlockItem
blockItemParser = do
  maybeType <- lookAhead $ optionMaybe $ try keyIntParser
  case maybeType of
    Just _ -> D <$> declarationParser
    _ ->  S <$> statementParser

functionDefineParser :: ParsecT String ParseInfo IO FunctionDefine
functionDefineParser = do
  retType <- try keyIntParser <|> keyVoidParser
  fName <- identifierParser
  argList <- between openPParser closePParser $ try argListParser
  parseInfo <- getState
  putState $ parseInfo {currentScopeVar = M.singleton varIdMapKey "1", precedence = lowestPrecedence}
  block <- blockParser
  nVarId <- read . (M.! varIdMapKey) <$> getCurrentScopeVar
  putState parseInfo
  pure $ FunctionDefine retType fName argList block nVarId

getLabelList :: [BlockItem] -> M.Map String String -> Either String (M.Map String String)
getLabelList [] m = Right m
getLabelList (bi : bis) m = case bi of
  S (Label l s) -> if M.member l m
                then Left $ "Label " ++ l ++ " already existed"
                else getLabelList (S s : bis) $ M.insert l l m
  S (If _ t (Just f)) -> getLabelList (S t : S f : bis) m
  S (If _ t _) -> getLabelList (S t : bis) m
  S (Compound (Block bl)) -> getLabelList (bl ++ bis) m
  _ -> getLabelList bis m

isValidGotoLabels :: [BlockItem] -> M.Map String String -> Either String (M.Map String String)
isValidGotoLabels [] m = Right m
isValidGotoLabels (bi : bis) m = case bi of
  S (Goto l) -> if not $ M.member l m
                  then Left $ "Goto label " ++ l ++ " not found"
                  else isValidGotoLabels bis m
  _ -> isValidGotoLabels bis m

labelCheck :: [FunctionDefine] -> Either [String] [M.Map String String]
labelCheck bls = do
  let labelList = map (flip getLabelList M.empty . unBlock . body) bls in
    if any isLeft labelList
      then Left $ map (fromLeft "") $ filter isLeft labelList
      else do
        let checkGoto = zipWith isValidGotoLabels
                          (map (unBlock . body) bls)
                          (map (fromRight M.empty) labelList) in
          if any isLeft checkGoto
            then Left $ map (fromLeft "") $ filter isLeft checkGoto
            else Right $ map (fromRight M.empty) checkGoto
