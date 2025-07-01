-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/07/01 17:32:38 by mayeung          ###   ########.fr       --
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
import Data.Maybe

type CProgramAST = [Declaration]

type VarType = String

type IdentifierName = String

data JumpLabel =
  LoopLabel (String, String, String)
  | SwitchLabel (Maybe String, String) (M.Map Int String)
  deriving (Show, Eq)

data PrimType =
  ITChar
  | ITInt
  | ITFloat
  | ITDouble
  deriving (Show, Eq)

data DT =
  DTInternal PrimType
  | DTUserDefined String [DT]
  | DTVoid
  deriving (Show, Eq)

data IdentifierType =
  VarIdentifier {vdt :: DT, vn :: IdentifierName, topLv :: Bool, varDefine :: Maybe Expr, storeClass :: Maybe StorageClass}
  | FuncIdentifier {fti :: FunTypeInfo}
  deriving (Show, Eq)

data ParseInfo = 
  ParseInfo
  {
    currentScopeIdent :: M.Map String IdentifierType,
    outerScopeIdent :: M.Map String IdentifierType,
    topLevelScopeIdent :: M.Map String IdentifierType,
    dataTypeMap :: M.Map String DT,
    precedence :: Int,
    labelId :: Int,
    currentVarId :: Int,
    outerVarId :: Int,
    jumpLabel :: [JumpLabel],
    topLevel :: Bool
  }
  deriving (Show, Eq)

data InputArgPair =
  ArgPair 
  {
    dataType :: DT,
    varName :: String
  }
  deriving (Show, Eq)

data ForInit =
  InitDecl VariableDeclaration
  | InitExpr (Maybe Expr)
  deriving (Show, Eq)

data Statement =
  Expression Expr
  | Return Expr
  | If Expr Statement (Maybe Statement)
  | Goto String
  | Label String String Statement
  | Compound Block
  | Break String
  | Continue String
  | While Expr Statement JumpLabel
  | DoWhile Statement Expr JumpLabel
  | For ForInit (Maybe Expr) (Maybe Expr) Statement JumpLabel
  | Switch Expr Statement JumpLabel
  | Case Statement String
  | Default Statement String
  | Null
  deriving (Show, Eq)

data Declaration =
  VD VariableDeclaration
  | FunctionDeclaration
    {
      funName :: String,
      inputArgs :: [InputArgPair],
      funRetType :: DT,
      funDefine :: Maybe Block,
      nextVarId :: Int,
      storageClass :: Maybe StorageClass
    }
  deriving (Show, Eq)

data VariableDeclaration =
  VariableDeclaration DT IdentifierName Bool (Maybe Expr) (Maybe StorageClass)
  deriving (Show, Eq)

data FunTypeInfo =
  FunTypeInfo
  {
    retType :: DT,
    fName :: IdentifierName,
    argumentList :: [DT],
    funBody :: Maybe Block
  }
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
  | FunctionCall String [Expr]
  | Unary UnaryOp Expr
  | Binary BinaryOp Expr Expr
  | Assignment BinaryOp Expr Expr
  | Conditional Expr Expr Expr
  deriving (Show, Eq)

data StorageClass =
  Static
  | Extern
  deriving Eq

instance Show StorageClass where
  show Static = "static"
  show Extern = "extern"

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
 "<<=", ">>=", "?", ","]

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
  ["int", "void", "return", "if",
  "else", "goto", "do", "while",
  "for", "break", "continue", "switch",
  "static", "extern"]

primDataTypeStrList :: [String]
primDataTypeStrList =
  ["char", "int", "float", "double"]

primTypeList :: [PrimType]
primTypeList = [ITChar, ITInt, ITFloat, ITDouble]

primTypeStringToPrimType :: String -> PrimType
primTypeStringToPrimType dt = M.fromList (zip primDataTypeStrList primTypeList) M.! dt 

primDataTypeMap :: M.Map String DT
primDataTypeMap = M.fromList $ ("void", DTVoid) : zip primDataTypeStrList (map DTInternal primTypeList)

defaultParsecState :: ParseInfo
defaultParsecState = ParseInfo
  {
    currentScopeIdent = M.empty,
    outerScopeIdent = M.empty,
    topLevelScopeIdent = M.empty,
    dataTypeMap = primDataTypeMap,
    precedence = lowestPrecedence,
    labelId = 1,
    currentVarId = 1,
    outerVarId = 1,
    jumpLabel = [],
    topLevel = True
  }

binaryOpPrecedence :: M.Map String Int
binaryOpPrecedence = M.fromList $ zip allBinaryOp
  [4, 4, 3, 3,
  3, 11, 12, 8,
  10, 5, 5, 9,
  7, 7, 6, 6,
  6, 6, 2, 14,
  14, 14, 14, 14,
  14, 14, 14, 14,
  14, 14, 13, 15]

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

keySwitchParser :: ParsecT String u IO String
keySwitchParser = keywordParserCreate "switch"

keyCaseParser :: ParsecT String u IO String
keyCaseParser = keywordParserCreate "case"

keyDefaultParser :: ParsecT String u IO String
keyDefaultParser = keywordParserCreate "default"

keyStaticParser :: ParsecT String u IO String
keyStaticParser = keywordParserCreate "static"

keyExternParser :: ParsecT String u IO String
keyExternParser = keywordParserCreate "extern"

identifierParser :: ParsecT String u IO String
identifierParser = do
  symbol <- symbolExtract
  if symbol `elem` keywordList
    then unexpected $ "Cannot use keyword as identifier: " ++ symbol
    else pure symbol

dataTypeParser :: ParsecT String ParseInfo IO DT
dataTypeParser = do
  dtName <- symbolExtract
  dtMap <- dataTypeMap <$> getState
  if M.member dtName dtMap && dtName /= "void"
    then pure $ dtMap M.! dtName
    else unexpected dtName

intParser :: ParsecT String u IO String
intParser = spaces >> many1 digit

fileParser :: ParsecT String ParseInfo IO [Declaration]
fileParser = manyTill declareParser $ try $ spaces >> eof

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

updateCurrentScopeIdent :: M.Map String IdentifierType -> ParseInfo -> ParseInfo
updateCurrentScopeIdent m parseInfo = parseInfo {currentScopeIdent = m}

updateOuterScopeVar :: M.Map String IdentifierType -> ParseInfo -> ParseInfo
updateOuterScopeVar m parseInfo = parseInfo {outerScopeIdent = m}

bumpLabelId :: ParsecT s ParseInfo IO ()
bumpLabelId = modifyState (\parseInfo -> parseInfo {labelId = labelId parseInfo + 1})

updateJumpLabel :: JumpLabel -> ParseInfo -> ParseInfo
updateJumpLabel jl parseInfo = parseInfo {jumpLabel = jl : jumpLabel parseInfo }

getCurrentScopeIdent :: ParsecT s ParseInfo IO (M.Map String IdentifierType)
getCurrentScopeIdent = currentScopeIdent <$> getState

getOuterScopeVar :: ParsecT s ParseInfo IO (M.Map String IdentifierType)
getOuterScopeVar = outerScopeIdent <$> getState

getPrecedence :: ParsecT s ParseInfo IO Int
getPrecedence = precedence <$> getState

getNextLabelId :: ParsecT s ParseInfo IO Int
getNextLabelId = labelId <$> getState

getJumpLabel :: ParsecT s ParseInfo IO [JumpLabel]
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
  current <- currentScopeIdent <$> getState
  outer <- outerScopeIdent <$> getState
  if not (M.member vName current) && not (M.member vName outer)
    then unexpected $ "Undefined variable: " ++ vName
    else if M.member vName current
      then do
        case current M.! vName of
          FuncIdentifier _ -> unexpected "Function identifer"
          _ -> postfixOp $ Variable (vn $ current M.! vName)
      else case outer M.! vName of
          FuncIdentifier _ -> unexpected "Function identifer"
          _ -> postfixOp $ Variable (vn $ outer M.! vName)

isFuncIdentiferInVarMap :: String -> M.Map String IdentifierType -> M.Map String IdentifierType -> Bool
isFuncIdentiferInVarMap fIdentifier current outer =
  (M.member fIdentifier current && isFuncIdentifier (current M.! fIdentifier))
    || (M.member fIdentifier outer && isFuncIdentifier (outer M.! fIdentifier))

getFunTypeInfoFromVarMap :: String -> M.Map String IdentifierType -> M.Map String IdentifierType -> FunTypeInfo
getFunTypeInfoFromVarMap fIdentifier current outer =
  if M.member fIdentifier current && isFuncIdentifier (current M.! fIdentifier)
    then case current M.! fIdentifier of
          FuncIdentifier fi -> fi
          _ -> undefined
    else case outer M.! fIdentifier of
          FuncIdentifier fi -> fi
          _ -> undefined

functionCallParser :: ParsecT String ParseInfo IO Expr
functionCallParser = do
  functionName <- identifierParser <* openPParser
  current <- currentScopeIdent <$> getState
  outer <- outerScopeIdent <$> getState
  if M.member functionName current && isVarIdentifier (current M.! functionName)
    then unexpected "using variable as function."
    else if not (isFuncIdentiferInVarMap functionName current outer)
      then unexpected $ functionName ++ ". Not declared before"
      else do
        maybeCloseParen <- lookAhead (try closePParser) <|> pure ""
        paraList <- case maybeCloseParen of
          ")" -> closePParser >> pure []
          _ -> do
            p <- precedence <$> getState
            modifyState $ updatePrecedence lowestPrecedence
            exprs <- sepBy exprParser (try commaParser)
            modifyState $ setPrecedence p
            void closePParser
            pure exprs
        let funcInfo = getFunTypeInfoFromVarMap functionName current outer
        if length paraList == length (argumentList funcInfo)
          then pure $ FunctionCall functionName paraList
          else unexpected "Function call. Incorrect number of parameters"

factorParser :: ParsecT String ParseInfo IO Expr
factorParser = do
  spaces
  c <- lookAhead anyChar
  next c
    where next c
            | isDigit c = intOperandParser
            | [c] `elem` allUnaryOp = unaryExprParser
            | c == '(' = parenExprParser
            | otherwise = do
                maybeParen <- lookAhead (try (identifierParser >> openPParser)) <|> pure ""
                case maybeParen of
                    "(" -> functionCallParser
                    _ -> variableParser

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

argPairParser :: ParsecT String ParseInfo IO InputArgPair
argPairParser = do
  dt <- dataTypeParser
  maybeNoName <- lookAhead (try closePParser <|> try commaParser) <|> pure ""
  case maybeNoName of
    "" -> do
      vName <- identifierParser
      current <- currentScopeIdent <$> getState
      if M.member vName current
        then unexpected $ vName ++ ". Already defined"
        else do
          modifyState (\p -> p {currentScopeIdent = M.insert vName (VarIdentifier dt vName False Nothing Nothing) (currentScopeIdent p)})
          pure $ ArgPair dt vName
    _ -> pure $ ArgPair dt ""

argListParser :: ParsecT String ParseInfo IO [InputArgPair]
argListParser = do
  res <- lookAhead (try closePParser <|> try symbolExtract) <|> pure ""
  case res of
    "void" -> keyVoidParser >> pure []
    ")" -> pure []
    _ -> sepBy argPairParser $ try commaParser

topLevelVarStorageClassCheck :: Maybe StorageClass -> Maybe StorageClass -> Bool
topLevelVarStorageClassCheck (Just Static) (Just Static) = True
topLevelVarStorageClassCheck (Just Static) _ = False
topLevelVarStorageClassCheck _ (Just Static) = False
topLevelVarStorageClassCheck _ _ = True

updateTopLevelVarCurrentVar :: DT -> IdentifierName -> Maybe Expr -> Maybe StorageClass -> ParsecT String ParseInfo IO ()
updateTopLevelVarCurrentVar dt vName vDf sClass = do
  topMap <- topLevelScopeIdent <$> getState
  let typeInfo = if M.member vName topMap
      then case storeClass $ topMap M.! vName of
        Just Extern -> VarIdentifier dt vName True vDf sClass
        _ -> case varDefine $ topMap M.! vName of
          og@(Just _) -> VarIdentifier dt vName True og sClass
          _ -> VarIdentifier dt vName True vDf sClass
      else VarIdentifier dt vName True vDf sClass
  modifyState (\p -> p {topLevelScopeIdent = M.insert vName typeInfo (topLevelScopeIdent p)}) 
  modifyState (\p -> p {currentScopeIdent = M.insert vName typeInfo (currentScopeIdent p)}) 

topLevelVarDeclarationParser :: ParsecT String ParseInfo IO VariableDeclaration
topLevelVarDeclarationParser = do
  sc1Try <- storageClassParser Nothing
  varType <- dataTypeParser
  sc <- storageClassParser sc1Try
  vName <- identifierParser <?> "Valid identifier"
  topIdentMap <- topLevelScopeIdent <$> getState
  if M.member vName topIdentMap
    then case topIdentMap M.! vName of
      FuncIdentifier _ -> unexpected $ vName ++ ". Already defined"
      VarIdentifier vType _ _ vDefine sClass -> if vType /= varType || not (topLevelVarStorageClassCheck sClass sc)
        then unexpected $ vName ++ ". Type mismatch to previous declaration"
        else do
          initialiser <- getinitialiser
          let newSClass = if sc == Just Extern then sClass else sc
          if isJust vDefine && isJust initialiser
            then unexpected "variable redefine"
            else do
              updateTopLevelVarCurrentVar vType vName initialiser sc 
              if isJust vDefine
                then pure $ VariableDeclaration vType vName True vDefine newSClass
                else pure $ VariableDeclaration vType vName True initialiser newSClass
    else do
      initialiser <- getinitialiser
      updateTopLevelVarCurrentVar varType vName initialiser sc
      pure $ VariableDeclaration varType vName True initialiser sc
    where getinitialiser = do
            maybeEqual <- optionMaybe $ try equalLex
            initialiser <- case maybeEqual of
              Just _ -> Just <$> intOperandParser
              _ -> pure Nothing
            void semiColParser
            pure initialiser

varDeclarationParser :: ParsecT String ParseInfo IO VariableDeclaration
varDeclarationParser = do
  sc1Try <- storageClassParser Nothing
  varType <- dataTypeParser
  sc <- storageClassParser sc1Try
  vName <- identifierParser <?> "Valid identifier"
  parseInfo <- getState
  if M.member vName $ currentScopeIdent parseInfo
    then unexpected $ "Variable redeclare: " ++ vName
    else do
      let varId = currentVarId parseInfo
          newVarName = vName ++ "#" ++ show varId
          newVarMap = M.insert vName (VarIdentifier varType newVarName (topLevel parseInfo) Nothing sc) $ currentScopeIdent parseInfo
      putState $ parseInfo {currentVarId = varId + 1, currentScopeIdent = newVarMap}
      maybeEqual <- optionMaybe $ try equalLex
      initialiser <-
        case maybeEqual of
          Just _ -> Just <$> exprParser
          _ -> pure Nothing
      void semiColParser
      pure $ VariableDeclaration varType newVarName (topLevel parseInfo) initialiser sc

expressionParser :: ParsecT String ParseInfo IO Statement
expressionParser = Expression <$> exprParser

nullStatParser :: ParsecT String ParseInfo IO Statement
nullStatParser = semiColParser >> pure Null

gotoParser :: ParsecT String ParseInfo IO Statement
gotoParser = keyGotoParser >> Goto <$> symbolExtract <* semiColParser

labelParser :: ParsecT String ParseInfo IO Statement
labelParser = do
  lName <- identifierParser
  newLabelId <- getNextLabelId <* bumpLabelId
  void colonParser
  stat <- statementParser <?> "Statement only"
  pure $ Label lName (lName ++ show newLabelId) stat

labelNameParser :: ParsecT String u IO String
labelNameParser = do
  lName <- identifierParser
  void colonParser
  pure lName

updateVarMapForNewScope :: ParseInfo -> M.Map String IdentifierType -> ParsecT String ParseInfo IO ()
updateVarMapForNewScope parseInfo m = do
  putState $ parseInfo
    {currentScopeIdent = m,
      outerScopeIdent = M.union (currentScopeIdent parseInfo) (outerScopeIdent parseInfo),
      precedence = lowestPrecedence}

keepIdsJumpLabel :: ParseInfo -> [JumpLabel] -> ParsecT s ParseInfo IO ()
keepIdsJumpLabel parseInfo jLabel = do
  newVarId <- currentVarId <$> getState
  lId <- labelId <$> getState
  putState $ parseInfo {currentVarId = newVarId, labelId = lId, jumpLabel = jLabel}

blockParser :: M.Map String IdentifierType -> ParsecT String ParseInfo IO Block
blockParser m = do
  void openCurParser
  parseInfo <- getState
  updateVarMapForNewScope parseInfo m
  modifyState $ \p -> p {topLevel = False}
  block <- manyTill blockItemParser $ try closeCurParser
  jLabel <- getJumpLabel
  current <- currentScopeIdent <$> getState
  keepIdsJumpLabel (parseInfo {outerScopeIdent = M.union (outerScopeIdent parseInfo) (M.filter isFuncIdentifier current)}) jLabel
  pure $ Block block

isSwitchLabel :: JumpLabel -> Bool
isSwitchLabel (SwitchLabel _ _) = True
isSwitchLabel _ = False

isLoopLabel :: JumpLabel -> Bool
isLoopLabel (LoopLabel _) = True
isLoopLabel _ = False

breakParser :: ParsecT String ParseInfo IO Statement
breakParser = do
  l <- getJumpLabel
  case l of
    [] -> unexpected "Break"
    LoopLabel (_, _, doneLabel) : _ -> keyBreakParser >> Break doneLabel <$ semiColParser
    SwitchLabel (_, doneLabel) _  : _ -> keyBreakParser >> Break doneLabel <$ semiColParser

continueParser :: ParsecT String ParseInfo IO Statement
continueParser = do
  l <- dropWhile isSwitchLabel <$> getJumpLabel
  case l of
    LoopLabel (_, continueLabel, _) : _ -> keyContinueParser >> Continue continueLabel <$ semiColParser
    _ -> unexpected "Continue"

makeLoopLabel :: ParsecT String ParseInfo IO ()
makeLoopLabel = do
  startLabel <- ("start" ++) . show <$> getNextLabelId <* bumpLabelId
  continueLabel <- ("continue" ++) . show <$> getNextLabelId <* bumpLabelId
  doneLabel <- ("done" ++) . show <$> getNextLabelId <* bumpLabelId
  modifyState $ updateJumpLabel $ LoopLabel (startLabel, continueLabel, doneLabel)

makeSwitchLabel :: ParsecT String ParseInfo IO ()
makeSwitchLabel = do
  doneLabel <- ("done" ++) . show <$> getNextLabelId <* bumpLabelId
  modifyState $ updateJumpLabel $ SwitchLabel (Nothing, doneLabel) M.empty

whileParser :: ParsecT String ParseInfo IO Statement
whileParser = do
  void keyWhileParser
  parseInfo <- getState
  updateVarMapForNewScope parseInfo M.empty
  makeLoopLabel
  condition <- parenExprParser
  whileBody <- statementParser
  jumpL <- getJumpLabel
  keepIdsJumpLabel parseInfo $ drop 1 jumpL
  pure $ While condition whileBody $ (!! 0) jumpL

doWhileParser :: ParsecT String ParseInfo IO Statement
doWhileParser = do
  void keyDoParser
  parseInfo <- getState
  updateVarMapForNewScope parseInfo M.empty
  makeLoopLabel
  doWhileBody <- statementParser
  void keyWhileParser
  condition <- parenExprParser <* semiColParser
  jumpL <- getJumpLabel
  keepIdsJumpLabel parseInfo $ drop 1 jumpL
  pure $ DoWhile doWhileBody condition $ (!! 0) jumpL

forInitParser :: ParsecT String ParseInfo IO ForInit
forInitParser = do
  maybeType <- lookAhead (try keyIntParser) <|> pure ""
  case maybeType of
    "int" -> InitDecl <$> varDeclarationParser
    _ -> InitExpr <$> optionMaybe exprParser <* semiColParser

forLoopParser :: ParsecT String ParseInfo IO Statement
forLoopParser = do
  void $ keyForParser >> openPParser
  parseInfo <- getState
  updateVarMapForNewScope parseInfo M.empty
  makeLoopLabel
  forInit <- forInitParser
  condition <- optionMaybe (try exprParser) <* semiColParser
  postBody <- optionMaybe (try exprParser) <* closePParser
  forBody <- statementParser
  jLabel <- getJumpLabel
  keepIdsJumpLabel parseInfo $ drop 1 jLabel
  pure $ For forInit condition postBody forBody $ (!! 0) jLabel

caseParser :: ParsecT String ParseInfo IO Statement
caseParser = do
  jL <- dropWhile isLoopLabel <$> getJumpLabel
  preJLabel <- takeWhile isLoopLabel <$> getJumpLabel
  case jL of
    SwitchLabel jLabel caseMap : lbs -> do
      void keyCaseParser
      val <- read <$> intParser
      if M.member val caseMap
        then unexpected $ show val ++ ": Already defined."
        else do
          void colonParser
          lId <- getNextLabelId <* bumpLabelId
          modifyState (\p -> p {
            jumpLabel = preJLabel ++ SwitchLabel jLabel (M.insert val ("case." ++ show val ++ "." ++ show lId) caseMap) : lbs})
          state <- statementParser
          pure $ Case state $ "case." ++ show val ++ "." ++ show lId
    _ -> unexpected "Case"

defaultParser :: ParsecT String ParseInfo IO Statement
defaultParser = do
  jL <- dropWhile isLoopLabel <$> getJumpLabel
  preJLabel <- takeWhile isLoopLabel <$> getJumpLabel
  case jL of
    SwitchLabel (Nothing, endLabel) caseMap : lbs -> do
      void $ keyDefaultParser >> colonParser
      lId <- getNextLabelId <* bumpLabelId
      modifyState (\p -> p {
        jumpLabel = preJLabel ++ SwitchLabel (Just $ "default" ++ show lId, endLabel) caseMap : lbs})
      state <- statementParser
      pure $ Default state $ "default" ++ show lId
    _ -> unexpected "Default"

switchParser :: ParsecT String ParseInfo IO Statement
switchParser = do
  parseInfo <- getState
  updateVarMapForNewScope parseInfo M.empty
  void keySwitchParser
  makeSwitchLabel
  expr <- parenExprParser
  bl <- statementParser
  jLabel <- getJumpLabel
  keepIdsJumpLabel parseInfo $ drop 1 jLabel
  pure $ Switch expr bl $ (!! 0) jLabel

statementParser :: ParsecT String ParseInfo IO Statement
statementParser = do
    sym <- lookAhead (try symbolExtract <|> try openCurParser <|> try semiColParser) <|> pure ""
    case sym of 
      "return" -> returnStatParser
      "if" -> ifStatParser
      ";" -> nullStatParser
      "goto" -> gotoParser
      "{" -> Compound <$> blockParser M.empty
      "break" -> breakParser
      "continue" -> continueParser
      "while" -> whileParser
      "do" -> doWhileParser
      "for" -> forLoopParser
      "case" -> caseParser
      "default" -> defaultParser
      "switch" -> switchParser
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
  maybeType <- lookAhead $ optionMaybe $ try keyIntParser <|> try storageClassStrParser
  case maybeType of
    Just _ -> D <$> declareParser
    _ -> S <$> statementParser

isValidDataType :: String -> ParsecT String ParseInfo IO Bool
isValidDataType typeName = do
  dtMap <- dataTypeMap <$> getState
  if M.member typeName dtMap
    then pure True
    else pure False

checkForFuncNameConflict :: String -> ParsecT String ParseInfo IO String
checkForFuncNameConflict iName = do
  current <- currentScopeIdent <$> getState
  if M.member iName current
    then case current M.! iName of
      VarIdentifier {} -> unexpected "Identifier redeclared"
      _ -> pure iName
    else pure iName

compareFunTypeDeclare :: FunTypeInfo -> FunTypeInfo -> Bool
compareFunTypeDeclare (FunTypeInfo lRt lFName lArgList _) (FunTypeInfo rRt rFName rArgList _) =
  lRt == rRt && lFName == rFName && lArgList == rArgList

checkForFuncTypeConflict :: ParseInfo -> Declaration -> ParsecT String ParseInfo IO [InputArgPair]
checkForFuncTypeConflict parseInfo n@(FunctionDeclaration fn aList rType _ _ _) = do
  let current = currentScopeIdent parseInfo
  let outer = outerScopeIdent parseInfo
  if M.member fn current
    then case current M.! fn of
      FuncIdentifier oldFuncTypeInfo ->
        if compareFunTypeDeclare
          (FunTypeInfo rType fn (map dataType aList) Nothing)
          oldFuncTypeInfo
          then pure $ inputArgs n
          else unexpected "Incompatible function type redeclare"
      _ -> unexpected "Identifier redeclared"
    else if M.member fn outer
      then case outer M.! fn of
        FuncIdentifier oldFuncTypeInfo ->
          if compareFunTypeDeclare
            (FunTypeInfo rType fn (map dataType aList) Nothing)
            oldFuncTypeInfo
            then pure $ inputArgs n
            else unexpected "Incompatible function type redeclare"
        _ -> pure $ inputArgs n
      else do
        modifyState (\p -> p {outerScopeIdent = M.insert fn (FuncIdentifier (FunTypeInfo rType fn (map dataType aList) Nothing)) (outerScopeIdent p)})
        pure $ inputArgs n
checkForFuncTypeConflict _ _ = undefined

isFuncIdentifier :: IdentifierType -> Bool
isFuncIdentifier (FuncIdentifier _) = True
isFuncIdentifier _ = False

isVarIdentifier :: IdentifierType -> Bool
isVarIdentifier (VarIdentifier {}) = True
isVarIdentifier _ = False

funcNotYetDefined :: String -> ParseInfo -> Bool
funcNotYetDefined name parseInfo =
  let current = currentScopeIdent parseInfo in
    M.member name current
      && isFuncIdentifier (current M.! name)
      && isNothing (funBody (fti (current M.! name)))

storageClassStrParser :: ParsecT String ParseInfo IO String
storageClassStrParser = try keyStaticParser <|> keyExternParser

storageClassParser :: Maybe StorageClass -> ParsecT String ParseInfo IO (Maybe StorageClass)
storageClassParser psc = do
  maybeSc <- optionMaybe $ try (keyStaticParser >> pure Static) <|> try (keyExternParser >> pure Extern)
  case maybeSc of
    Nothing -> pure psc
    Just sc -> case psc of
      Nothing -> pure $ Just sc
      Just _ -> unexpected $ show sc

functionDeclareParser :: ParsecT String ParseInfo IO Declaration
functionDeclareParser = do
  sc1Try <- storageClassParser Nothing
  rType <- dataTypeParser
  sc <- storageClassParser sc1Try
  name <- identifierParser >>= checkForFuncNameConflict
  parseInfo <- getState
  toplvl <- topLevel <$> getState
  modifyState (\p -> p {outerScopeIdent = M.union (outerScopeIdent p) (currentScopeIdent p),
    currentScopeIdent = M.empty, currentVarId = if toplvl then 1 else currentVarId p, precedence = lowestPrecedence})
  argList <- between openPParser closePParser (try argListParser)
    >>= checkForFuncTypeConflict parseInfo .
      (\aList -> FunctionDeclaration name aList rType Nothing 1 undefined)
  unless
    (M.member name (outerScopeIdent parseInfo)) $
    modifyState (\p -> p {outerScopeIdent = M.insert name (FuncIdentifier (FunTypeInfo rType name (map dataType argList) Nothing)) (outerScopeIdent p)})
  maybeSemiColon <- lookAhead (try semiColParser <|> try openCurParser)
  block <- case maybeSemiColon of
    ";" -> semiColParser >> pure Nothing
    "{" -> if topLevel parseInfo
      then do
        let current = currentScopeIdent parseInfo
        if M.member name current
          then case current M.! name of
            FuncIdentifier (FunTypeInfo _ _ _ (Just _)) -> unexpected "Function redefinition"
            _ -> Just <$> blockParser (M.fromList (map (\(ArgPair dt vName) -> (vName, VarIdentifier dt vName toplvl Nothing sc)) argList))
          else
            Just <$> blockParser (M.fromList (map (\(ArgPair dt vName) -> (vName, VarIdentifier dt vName toplvl Nothing sc)) argList))
      else unexpected "Function definition"
    _ -> unexpected maybeSemiColon
  nVarId <- currentVarId <$> getState
  jLabel <- getJumpLabel
  current <- currentScopeIdent <$> getState
  outer <- outerScopeIdent <$> getState
  keepIdsJumpLabel
    (parseInfo {outerScopeIdent = M.union outer (M.filter isFuncIdentifier current)})
    jLabel
  s <- getState
  unless (funcNotYetDefined name s) $
    modifyState (\p -> p {currentScopeIdent = M.insert name (FuncIdentifier (FunTypeInfo rType name (map dataType argList) block)) (currentScopeIdent p)})
  unless (funcNotYetDefined name s) $
    modifyState (\p -> p {outerScopeIdent = M.insert name (FuncIdentifier (FunTypeInfo rType name (map dataType argList) block)) (outerScopeIdent p)})
  pure $ FunctionDeclaration name  argList rType block nVarId sc

declareParser :: ParsecT String ParseInfo IO Declaration
declareParser = do
  maybeParen <- lookAhead (try $ storageClassParser Nothing >> dataTypeParser >> storageClassParser Nothing >> identifierParser >> openPParser) <|> pure ""
  toplvl <- topLevel <$> getState
  if maybeParen == "("
    then functionDeclareParser
    else if toplvl
      then VD <$> topLevelVarDeclarationParser
      else VD <$> varDeclarationParser

getLabelList :: [BlockItem] -> M.Map String String -> Either String (M.Map String String)
getLabelList [] m = Right m
getLabelList (bi : bis) m = case bi of
  S (Label og l s) -> if M.member og m
                then Left $ "Label " ++ l ++ " already existed"
                else getLabelList (S s : bis) $ M.insert og l  m
  S (If _ t (Just f)) -> getLabelList (S t : S f : bis) m
  S (If _ t _) -> getLabelList (S t : bis) m
  S (Compound (Block bl)) -> getLabelList (bl ++ bis) m
  S (For _ _ _ s _) -> getLabelList (S s : bis) m
  S (Switch _ s _) -> getLabelList (S s : bis) m
  S (While _ s _) -> getLabelList (S s : bis) m
  S (DoWhile s _ _) -> getLabelList (S s : bis) m
  S (Case s _) -> getLabelList (S s : bis) m
  S (Default s _) -> getLabelList (S s : bis) m
  _ -> getLabelList bis m

isValidGotoLabels :: [BlockItem] -> M.Map String String -> Either String (M.Map String String)
isValidGotoLabels [] m = Right m
isValidGotoLabels (bi : bis) m = case bi of
  S (Goto l) -> if not $ M.member l m
                  then Left $ "Goto label " ++ l ++ " not found"
                  else isValidGotoLabels bis m
  S (Compound (Block bl)) -> isValidGotoLabels (bl ++ bis) m
  S (For _ _ _ s _) -> isValidGotoLabels (S s : bis) m
  S (Switch _ s _) -> isValidGotoLabels (S s : bis) m
  S (While _ s _) -> isValidGotoLabels (S s : bis) m
  S (DoWhile s _ _) -> isValidGotoLabels (S s : bis) m
  S (Case s _) -> isValidGotoLabels (S s : bis) m
  S (Default s _) -> isValidGotoLabels (S s : bis) m
  _ -> isValidGotoLabels bis m

labelCheck :: [[BlockItem]] -> Either [String] [M.Map String String]
labelCheck bls = do
  let labelList = map (`getLabelList` M.empty) bls
  if any isLeft labelList
    then Left $ map (fromLeft "") $ filter isLeft labelList
    else do
      let checkGoto = zipWith isValidGotoLabels
                        bls
                        (map (fromRight M.empty) labelList)
      if any isLeft checkGoto
        then Left $ map (fromLeft "") $ filter isLeft checkGoto
        else Right $ map (fromRight M.empty) checkGoto

extractStatement :: BlockItem -> Statement
extractStatement (S s) = s
extractStatement _ = undefined

updateLabelBlockItem :: BlockItem -> M.Map String String -> BlockItem
updateLabelBlockItem bi m = case bi of
  S (Label og l s) -> S (Label og l (extractStatement (updateLabelBlockItem (S s) m)))
  S (Goto l) -> S (Goto (m M.! l))
  S (If e t (Just f)) -> S (If e (extractStatement (updateLabelBlockItem (S t) m))
    (Just (extractStatement (updateLabelBlockItem (S f) m))))
  S (If e t f) -> S (If e (extractStatement (updateLabelBlockItem (S t) m)) f)
  S (Compound (Block bl)) -> S (Compound (Block (map (`updateLabelBlockItem` m) bl)))
  S (For i e p s l) -> S (For i e p (extractStatement (updateLabelBlockItem (S s) m)) l)
  S (Switch e s l) -> S (Switch e (extractStatement (updateLabelBlockItem (S s) m)) l)
  S (While e s l) -> S (While e (extractStatement (updateLabelBlockItem (S s) m)) l)
  S (DoWhile s e l) -> S (DoWhile (extractStatement (updateLabelBlockItem (S s) m)) e l)
  S (Case s l) -> S (Case (extractStatement (updateLabelBlockItem (S s) m)) l)
  S (Default s l) -> S (Default (extractStatement (updateLabelBlockItem (S s) m)) l)
  _ -> bi

updateLabelSingle :: Declaration -> M.Map String String -> Declaration
updateLabelSingle vd@(VD _) _ = vd
updateLabelSingle fd@(FunctionDeclaration _ _ _ Nothing _ _) _ = fd
updateLabelSingle (FunctionDeclaration n args rt (Just (Block bls)) lid sc) m =
  FunctionDeclaration n args rt (Just (Block (map (`updateLabelBlockItem` m) bls))) lid sc

updateGotoLabel :: [Declaration] -> [M.Map String String] -> [Declaration]
updateGotoLabel = zipWith updateLabelSingle
