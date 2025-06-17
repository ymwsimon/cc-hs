-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/06/17 22:57:51 by mayeung          ###   ########.fr       --
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

data FunctionDefine =
  FunctionDefine
  {
    returnType :: String,
    funName :: String,
    inputArgs :: [InputArgPair],
    body :: [BlockItem],
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

data Statement =
  Expression Expr
  | Return Expr
  | If Expr BlockItem (Maybe BlockItem)
  | Goto String
  | Null
  deriving (Show, Eq)

data Declaration =
  VariableDecl VarType IdentifierName (Maybe Expr)
  deriving (Show, Eq)

data BlockItem =
  S Statement
  | D Declaration
  | L String BlockItem
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
  ["int", "void", "return", "if", "else", "goto"]

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

identifierParser :: ParsecT String u IO String
identifierParser = do
  symbol <- symbolExtract
  if symbol `elem` keywordList
    then unexpected $ "Cannot use keyword as identifier: " ++ symbol
    else pure symbol

intParser :: ParsecT String u IO String
intParser = spaces >> many1 digit

fileParser :: ParsecT String (M.Map String String, Int) IO [FunctionDefine]
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

binaryExprParser :: ParsecT String (M.Map String String, Int) IO Expr
binaryExprParser = flip Binary
  <$> factorParser
  <*> binaryOpParser
  <*> factorParser

unaryExprParser :: ParsecT String (M.Map String String, Int) IO Expr
unaryExprParser = do
  uOpStr <- lookAhead unaryOpStringParser
  uOp <- unaryOpParser
  p <- snd <$> getState
  modifyState $ updatePrecedence $ getUnaryOpPrecedence uOpStr
  expr <- exprParser
  if uOp `elem` [PreDecrement, PreIncrement]
    then case expr of
      Variable _ -> Unary uOp expr <$ modifyState (setPrecedence p)
      _ -> unexpected "Need lvalue for prefix operation"
    else Unary uOp expr <$ modifyState (setPrecedence p)

exprParser :: ParsecT String (M.Map String String, Int) IO Expr
exprParser = factorParser >>= exprRightParser

isBinaryOpChar :: String -> Bool
isBinaryOpChar = flip elem allBinaryOp

getBinOpPrecedence :: String -> Int
getBinOpPrecedence = (binaryOpPrecedence M.!)

getUnaryOpPrecedence :: String -> Int
getUnaryOpPrecedence = (unaryOpPrecedence M.!)

updatePrecedence :: Num b => b -> (a, c) -> (a, b)
updatePrecedence p = (, p - 1) . fst

setPrecedence :: b -> (a, c) -> (a, b)
setPrecedence p = (, p) . fst

isEqOrHigherPrecedence :: String -> Int -> Bool
isEqOrHigherPrecedence binOp p = (binaryOpPrecedence M.! binOp) <= p

condtionalTrueParser :: ParsecT String (M.Map String String, Int) IO Expr
condtionalTrueParser = do
  void questionParser
  oldState <- snd <$> getState
  modifyState $ updatePrecedence lowestPrecedence
  expr <- exprParser
  modifyState $ setPrecedence oldState
  void colonParser
  pure expr

exprRightParser :: Expr -> ParsecT String (M.Map String String, Int) IO Expr
exprRightParser lExpr = do
  binOp <- lookAhead (try binaryOpStringParser) <|> pure ""
  p <- snd <$> getState
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

parenExprParser :: ParsecT String (M.Map String String, Int) IO Expr
parenExprParser = do
  void openPParser
  p <- snd <$> getState
  modifyState $ updatePrecedence lowestPrecedence
  expr <- exprParser
  void closePParser
  modifyState $ setPrecedence p
  postfixOp expr

variableParser :: ParsecT String (M.Map String String, Int) IO Expr
variableParser = do
  vName <- identifierParser
  varMap <- fst <$> getState
  if not $ M.member vName varMap
    then unexpected $ "Undefined variable: " ++ vName
    else postfixOp $ Variable (varMap M.! vName)

factorParser :: ParsecT String (M.Map String String, Int) IO Expr
factorParser = do
  spaces
  c <- lookAhead anyChar
  next c
    where next c
            | isDigit c = intOperandParser
            | [c] `elem` allUnaryOp = unaryExprParser
            | c == '(' = parenExprParser
            | otherwise = variableParser

returnStatParser :: ParsecT String (M.Map String String, Int) IO Statement
returnStatParser = do
  void keyReturnParser
  expr <- exprParser
  void semiColParser
  pure $ Return expr

isStatement :: BlockItem -> Bool
isStatement bi = case bi of
  L _ i -> isStatement i
  S _ -> True
  _ -> False

ifStatParser :: ParsecT String (M.Map String String, Int) IO Statement
ifStatParser = do
  void keyIfParser
  cond <- parenExprParser
  tStat <- blockItemParser
  if not $ isStatement tStat
    then unexpected ""
    else do
      maybeElse <- lookAhead $ try keyElseParser <|> pure ""
      fStat <- case maybeElse of
        "else" -> do
          void keyElseParser
          fs <- blockItemParser
          if not $ isStatement fs
            then unexpected ""
            else pure $ Just fs
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

declarationParser :: ParsecT String (M.Map String String, Int) IO Declaration
declarationParser = do
  varType <- keyIntParser
  vName <- try identifierParser <?> "Valid identifier"
  (varMap, p) <- getState
  if M.member vName varMap
    then unexpected $ "Variable redeclare: " ++ vName
    else do
      let varId = read $ varMap M.! varIdMapKey
          newVarId = (+ (1 :: Int)) varId
          newVarName = vName ++ "#" ++ show varId
          newVarMap = M.adjust (const (show newVarId)) varIdMapKey varMap
      putState (M.insert vName newVarName newVarMap, p)
      maybeEqual <- optionMaybe $ try equalLex
      initialiser <-
        case maybeEqual of
          Just _ -> Just <$> exprParser
          _ -> pure Nothing
      void semiColParser
      pure $ VariableDecl varType newVarName initialiser

expressionParser :: ParsecT String (M.Map String String, Int) IO Statement
expressionParser = Expression <$> exprParser

nullStatParser :: ParsecT String (ds, Int) IO Statement
nullStatParser = semiColParser >> pure Null

gotoParser :: ParsecT String (ds, Int) IO Statement
gotoParser = keyGotoParser >> Goto <$> symbolExtract <* semiColParser

statementParser :: ParsecT String (M.Map String String, Int) IO Statement
statementParser = (try nullStatParser <?> "Null statement") <|>
  do
    sym <- lookAhead (try symbolExtract) <|> pure ""
    case sym of 
      "return" -> returnStatParser
      "if" -> ifStatParser
      "goto" -> gotoParser
      _ -> expressionParser <* semiColParser

blockItemParser :: ParsecT String (M.Map String String, Int) IO BlockItem
blockItemParser = do
  maybeLabel <- lookAhead $ try (identifierParser <* colonParser) <|> pure ""
  if null maybeLabel
    then do
      maybeType <- lookAhead $ optionMaybe $ try keyIntParser
      case maybeType of
        Just _ -> D <$> declarationParser
        _ ->  S <$> statementParser
    else
      identifierParser >> colonParser >> L maybeLabel <$> blockItemParser

functionDefineParser :: ParsecT String (M.Map String String, Int) IO FunctionDefine
functionDefineParser = do
  retType <- try keyIntParser <|> keyVoidParser
  fName <- identifierParser
  argList <- between openPParser closePParser $ try argListParser
  (ogVarMap, p) <- getState
  putState (M.adjust (const "1") varIdMapKey ogVarMap, p)
  void openCurParser
  blockitems <- manyTill blockItemParser $ try $ spaces >> closeCurParser
  varMap <- fst <$> getState
  putState (ogVarMap, p)
  pure $ FunctionDefine retType fName argList blockitems $ read $ varMap M.! varIdMapKey

getLabelList :: [BlockItem] -> M.Map String String -> Either String (M.Map String String)
getLabelList [] m = Right m
getLabelList (bi : bis) m = case bi of
  L l b -> if M.member l m
                then Left $ "Label " ++ l ++ " already existed"
                else getLabelList (b : bis) $ M.insert l l m
  S (If _ t (Just f)) -> getLabelList (t : f : bis) m
  S (If _ t _) -> getLabelList (t : bis) m
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
  let labelList = map (flip getLabelList M.empty . body) bls in
    if any isLeft labelList
      then Left $ map (fromLeft "") $ filter isLeft labelList
      else do
        let checkGoto = zipWith isValidGotoLabels (map body bls) (map (fromRight M.empty) labelList) in
          if any isLeft checkGoto
            then Left $ map (fromLeft "") $ filter isLeft checkGoto
            else Right $ map (fromRight M.empty) checkGoto
