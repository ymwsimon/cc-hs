-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/07/06 19:05:33 by mayeung          ###   ########.fr       --
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
import Data.List (sort)
import Data.Int

type CProgramAST = [Declaration]

type VarType = String

type IdentifierName = String

data JumpLabel =
  LoopLabel (String, String, String)
  | SwitchLabel (Maybe String, String) (M.Map Int64 String)
  deriving (Show, Eq)

data PrimType =
  TVoid
  | TChar
  | TUChar
  | TShort
  | TUShort
  | TInt
  | TUInt
  | TLong
  | TULong
  | TFloat
  | TDouble
  | TLDouble
  deriving (Show, Eq)

data DT =
  DTInternal PrimType
  | DTFuncType {argList :: [DT], retType :: DT}
  | DTUserDefined String [DT]
  deriving (Show, Eq)

data IdentifierType =
  VarIdentifier {vdt :: DT, vn :: IdentifierName, topLv :: Bool,
    varDefine :: Maybe Expr, storeClass :: Maybe StorageClass}
  | FuncIdentifier {fti :: FunTypeInfo}
  deriving (Show, Eq)

data ParseInfo = 
  ParseInfo
  {
    currentScopeIdent :: M.Map String IdentifierType,
    outerScopeIdent :: M.Map String IdentifierType,
    topLevelScopeIdent :: M.Map String IdentifierType,
    dataTypeMap :: M.Map [String] DT,
    precedence :: Int,
    labelId :: Int,
    currentVarId :: Int,
    globalVarId :: Int,
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
      funcType :: DT,
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
    funType :: DT,
    fName :: IdentifierName,
    funBody :: Maybe Block,
    funcStorageClass :: Maybe StorageClass
  }
  deriving (Show, Eq)

data BlockItem =
  S Statement
  | D Declaration
  deriving (Show, Eq)

newtype Block = Block {unBlock :: [BlockItem]}
  deriving (Show, Eq)

data Expr =
  Constant NumConst
  | Variable String Bool (Maybe StorageClass)
  | Cast DT Expr
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

data NumConst =
  ConstInt Int
  | ConstLong Int64
  deriving (Show, Eq)

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
  ["int", "long", "short", "char", "float", "double",
    "void", "signed", "unsigned",
    "return", "if", "else", "goto", "do", "while",
    "for", "break", "continue", "switch",
    "static", "extern", "auto", "register"]

primDataTypeStrList :: [[String]]
primDataTypeStrList = map sort
  [["void"], ["char"], ["signed", "char"], ["unsigned", "char"],
    ["short"], ["signed", "short"], ["unsigned", "short"],
    ["int"], ["signed", "int"], ["unsigned", "int"],
    ["long"], ["long", "int"], ["signed", "long"], ["signed", "long", "int"],
    ["long", "long"], ["long", "long", "int"], ["signed", "long", "long"], 
    ["signed", "long", "long", "int"],
    ["unsigned", "long"], ["unsigned", "long", "long"],
    ["float"], ["double"], ["long", "double"]]

primTypeList :: [PrimType]
primTypeList =
  [TVoid, TChar, TChar, TUChar,
    TShort, TShort, TUShort,
    TInt, TInt, TUInt,
    TLong, TLong, TLong, TLong,
    TLong, TLong, TLong, TLong,
    TULong, TULong,
    TFloat, TDouble, TLDouble]

typeSpecifierStrList :: [String]
typeSpecifierStrList =
  ["void", "char", "int", "short", "long", "signed", "unsigned", "float", "double"]

storageClassSpecifierStrList :: [String]
storageClassSpecifierStrList = ["static", "extern"]

primTypeStringToPrimType :: String -> PrimType
primTypeStringToPrimType dt = M.fromList (zip primDataTypeStrList primTypeList) M.! [dt] 

primDataTypeMap :: M.Map [String] DT
primDataTypeMap = M.fromList $ zip primDataTypeStrList (map DTInternal primTypeList)

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
    globalVarId = 1,
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

declarationSpecifierStrList :: [String]
declarationSpecifierStrList = typeSpecifierStrList ++ storageClassSpecifierStrList

declarationSpecifierParser :: Bool -> ([String], [String]) -> ParsecT String ParseInfo IO (Maybe StorageClass, DT)
declarationSpecifierParser isFunc (toks, prohibitedList) = do
  specifier <- lookAhead symbolExtract
  when (specifier `elem` declarationSpecifierStrList
    && specifier `elem` prohibitedList) $
    unexpected specifier
  toplvl <- topLevel <$> getState
  when (isFunc && not toplvl && specifier == "static") $
    unexpected specifier
  if specifier `notElem` declarationSpecifierStrList
    then do
      unless (any (`notElem` storageClassSpecifierStrList) toks) $
        unexpected "identifier. Need type specifier"
      let sc = case filter (`elem` storageClassSpecifierStrList) toks of
            [] -> Nothing 
            ["static"] -> Just Static
            ["extern"] -> Just Extern
            _ -> undefined
      pure (sc, primDataTypeMap M.! sort (filter (`notElem` storageClassSpecifierStrList) toks))
    else
      let newPBList =
           (if (== 2) $ length $ filter (== "long") (specifier : toks)
            then ["void", "char", "short", "long", "float", "double"]
            else []) ++
            concatMap prohibitedNextTokensList (specifier : toks) in
      symbolExtract >> declarationSpecifierParser isFunc (specifier : toks, newPBList)

prohibitedNextTokensList :: String -> [String]
prohibitedNextTokensList tok = case tok of
  "void" -> ["void", "char", "int", "short", "long", "signed", "unsigned", "float", "double"]
  "char" -> ["void", "char", "int", "short", "long", "float", "double"]
  "int" -> ["void", "char", "int", "float", "double"]
  "short" -> ["void", "char", "short", "long", "float", "double"]
  "long" -> ["void", "char", "short", "float"]
  "signed" -> ["void", "signed", "unsigned", "float", "double"]
  "unsigned" -> ["void", "signed", "unsigned", "float", "double"]
  "float" -> ["void", "char", "int", "short", "long", "signed", "unsigned", "float", "double"]
  "double" -> ["void", "char", "int", "short", "signed", "unsigned", "float", "double"]
  "static" -> ["static", "extern"]
  "extern" -> ["static", "extern"]
  _ -> []

dataTypeParser :: ParsecT String ParseInfo IO DT
dataTypeParser = do
  dtName <- symbolExtract
  dtMap <- dataTypeMap <$> getState
  if M.member [dtName] dtMap && dtName /= "void"
    then pure $ dtMap M.! [dtName]
    else unexpected dtName

intParser :: ParsecT String u IO String
intParser = spaces >> many1 digit

longParser :: ParsecT String u IO String
longParser = spaces >> many1 digit <* oneOf "lL"

fileParser :: ParsecT String ParseInfo IO (M.Map String IdentifierType, [Declaration])
fileParser = do
  declares <- manyTill declareParser $ try $ spaces >> eof
  top <- topLevelScopeIdent <$> getState
  pure (top, declares)

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

exprToInt :: Expr -> Int64
exprToInt expr = case expr of
  Constant (ConstInt i) -> fromIntegral i
  Constant (ConstLong i) -> i
  _ -> 0

isVariableExpr :: Expr -> Bool
isVariableExpr (Variable {}) = True
isVariableExpr _ = False

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
  when (uOp `elem` [PreDecrement, PreIncrement] && not (isVariableExpr expr)) $
    unexpected "Need lvalue for prefix operation"
  Unary uOp expr <$ modifyState (setPrecedence p)

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
                Variable {} -> do
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
intOperandParser = Constant . ConstInt . read <$> intParser

longOperandParser :: ParsecT String u IO Expr
longOperandParser = Constant . ConstLong . read <$> longParser

postfixOp :: Expr -> ParsecT String u IO Expr
postfixOp expr = case expr of
    Variable {} -> do
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
  when (not (M.member vName current) && not (M.member vName outer)) $
    unexpected $ "Undefined variable: " ++ vName
  when ((M.member vName current && isFuncIdentifier (current M.! vName)) ||
      (not (M.member vName current) && M.member vName outer &&
        isFuncIdentifier (outer M.! vName))) $
    unexpected "Function identifier"
  if M.member vName current
    then let var = current M.! vName in
        postfixOp $ Variable (vn var) (topLv var) (storeClass var) 
    else let var = outer M.! vName in
        postfixOp $ Variable (vn var) (topLv var) (storeClass var)

isFuncIdentiferInVarMap :: String -> M.Map String IdentifierType -> M.Map String IdentifierType -> Bool
isFuncIdentiferInVarMap fIdentifier current outer =
  (M.member fIdentifier current && isFuncIdentifier (current M.! fIdentifier))
    || (M.member fIdentifier outer && isFuncIdentifier (outer M.! fIdentifier))

getFunTypeInfoFromVarMap :: String -> M.Map String IdentifierType -> M.Map String IdentifierType -> FunTypeInfo
getFunTypeInfoFromVarMap fIdentifier current outer =
  if M.member fIdentifier current && isFuncIdentifier (current M.! fIdentifier)
    then fti $ current M.! fIdentifier
    else fti $ outer M.! fIdentifier

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
        if length paraList == length (argList $ funType funcInfo)
          then pure $ FunctionCall functionName paraList
          else unexpected "Function call. Incorrect number of parameters"

factorParser :: ParsecT String ParseInfo IO Expr
factorParser = do
  spaces
  c <- lookAhead anyChar
  next c where
    next c
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
          modifyState (\p -> p {currentScopeIdent =
            M.insert vName (VarIdentifier dt vName False Nothing Nothing) (currentScopeIdent p)})
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
topLevelVarStorageClassCheck (Just Static) (Just Extern) = True
topLevelVarStorageClassCheck (Just Static) Nothing = False
topLevelVarStorageClassCheck _ (Just Static) = False
topLevelVarStorageClassCheck _ _ = True

topLevelFuncStorageClassCheck :: Maybe StorageClass -> Maybe StorageClass -> Bool
topLevelFuncStorageClassCheck (Just Static) (Just Static) = True
topLevelFuncStorageClassCheck (Just Static) (Just Extern) = True
topLevelFuncStorageClassCheck (Just Static) Nothing = True
topLevelFuncStorageClassCheck _ (Just Static) = False
topLevelFuncStorageClassCheck _ _ = True

updateTopLevelVarCurrentVar :: DT -> IdentifierName -> Maybe Expr ->
  Maybe StorageClass -> ParsecT String ParseInfo IO ()
updateTopLevelVarCurrentVar dt vName vDf sClass = do
  topMap <- topLevelScopeIdent <$> getState
  let typeInfo = if M.member vName topMap
      then case storeClass $ topMap M.! vName of
        Just Extern -> VarIdentifier dt vName True vDf sClass
        ogSClass -> case varDefine $ topMap M.! vName of
          og@(Just _) -> VarIdentifier dt vName True og ogSClass
          _ -> VarIdentifier dt vName True vDf ogSClass
      else VarIdentifier dt vName True vDf sClass
  modifyState (\p -> p {topLevelScopeIdent = M.insert vName typeInfo (topLevelScopeIdent p)})
  modifyState (\p -> p {currentScopeIdent = M.insert vName typeInfo (currentScopeIdent p)})

getInitialiser :: ParsecT String u IO Expr -> ParsecT String u IO (Maybe Expr)
getInitialiser parser = do
  maybeEqual <- optionMaybe $ try equalLex
  initialiser <- case maybeEqual of
    Just _ -> Just <$> parser
    _ -> pure Nothing
  void semiColParser
  pure initialiser

topLevelVarDeclarationParser :: ParsecT String ParseInfo IO VariableDeclaration
topLevelVarDeclarationParser = do
  (sc, varType) <- declarationSpecifierParser False ([], [])
  vName <- identifierParser <?> "Valid identifier"
  topIdentMap <- topLevelScopeIdent <$> getState
  when (M.member vName topIdentMap && isFuncIdentifier (topIdentMap M.! vName)) $
    unexpected $ vName ++ ". Already defined"
  when (M.member vName topIdentMap && isVarIdentifier (topIdentMap M.! vName)
    && (vdt (topIdentMap M.! vName) /= varType
      || not (topLevelVarStorageClassCheck (storeClass (topIdentMap M.! vName)) sc))) $
    unexpected $ vName ++ ". Type mismatch to previous declaration"
  initialiser <- getInitialiser intOperandParser
  if M.member vName topIdentMap
    then do
      VarIdentifier vType _ _ vDefine sClass <- pure $ topIdentMap M.! vName
      when (isJust vDefine && isJust initialiser) $
        unexpected "variable redefine"
      let newSClass = if sc == Just Extern then sClass else sc
      let define = if isJust vDefine then vDefine else initialiser
      updateTopLevelVarCurrentVar vType vName initialiser sc
      pure $ VariableDeclaration vType vName True define newSClass
    else do
      updateTopLevelVarCurrentVar varType vName initialiser sc
      pure $ VariableDeclaration varType vName True initialiser sc

localExternVarHandle :: DT -> Maybe StorageClass -> IdentifierName -> ParsecT String ParseInfo IO VariableDeclaration
localExternVarHandle varType sc vName  = do
  void semiColParser
  parseInfo <- getState
  unless (M.member vName $ topLevelScopeIdent parseInfo)
      $ updateTopLevelVarCurrentVar varType vName Nothing sc
  modifyState (\p -> p {currentScopeIdent = M.insert vName
    (VarIdentifier varType vName (topLevel p) Nothing sc) $ currentScopeIdent p})
  pure $ VariableDeclaration varType vName (topLevel parseInfo) Nothing sc

localPlainVarHandle :: DT -> Maybe StorageClass -> IdentifierName -> ParsecT String ParseInfo IO VariableDeclaration
localPlainVarHandle varType sc vName = do
  parseInfo <- getState
  let varId = currentVarId parseInfo
      newVarName = vName ++ "#" ++ show varId
      newVarMap = M.insert vName
        (VarIdentifier varType newVarName (topLevel parseInfo) Nothing sc) $
        currentScopeIdent parseInfo
  putState $ parseInfo {currentVarId = varId + 1, currentScopeIdent = newVarMap}
  initialiser <- getInitialiser exprParser
  pure $ VariableDeclaration varType newVarName (topLevel parseInfo) initialiser sc

localStaticVarHandle :: DT -> Maybe StorageClass -> IdentifierName -> ParsecT String ParseInfo IO VariableDeclaration
localStaticVarHandle varType sc vName = do
  initialiser <- getInitialiser intOperandParser
  parseInfo <- getState
  let varId = globalVarId parseInfo
      newVarName = vName ++ "." ++ show varId
      newLocalVarMap = M.insert vName
        (VarIdentifier varType newVarName (topLevel parseInfo) Nothing sc) $
        currentScopeIdent parseInfo
      newGlobalVarMap = M.insert newVarName
        (VarIdentifier varType newVarName (topLevel parseInfo) initialiser sc) $
        topLevelScopeIdent parseInfo
  putState $ parseInfo {globalVarId = varId + 1,
    currentScopeIdent = newLocalVarMap, topLevelScopeIdent = newGlobalVarMap}
  pure $ VariableDeclaration varType newVarName (topLevel parseInfo) initialiser sc

varDeclarationParser :: ParsecT String ParseInfo IO VariableDeclaration
varDeclarationParser = do
  (sc, varType) <- declarationSpecifierParser False ([], [])
  vName <- identifierParser <?> "Valid identifier"
  current <- currentScopeIdent <$> getState
  topIdentMap <- topLevelScopeIdent <$> getState
  when (M.member vName current
    && (isFuncIdentifier (current M.! vName)
      || (storeClass (current M.! vName) /= Just Extern) || sc /= Just Extern)) $
    unexpected $ "Variable redeclare: " ++ vName
  when (sc == Just Extern && M.member vName topIdentMap && isFuncIdentifier (topIdentMap M.! vName)) $
    unexpected $ vName ++ ". Already defined"
  if sc == Just Extern
    then localExternVarHandle varType sc vName
    else if isNothing sc
      then localPlainVarHandle varType sc vName
    else localStaticVarHandle varType sc vName

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
      outerScopeIdent = M.union (currentScopeIdent parseInfo) (outerScopeIdent parseInfo)}

keepIdsJumpLabel :: ParseInfo -> (M.Map String IdentifierType -> M.Map String IdentifierType) ->
  ([JumpLabel] -> [JumpLabel]) -> ParsecT String ParseInfo IO ()
keepIdsJumpLabel parseInfo outerExtract jLabelExtract = do
  newVarId <- currentVarId <$> getState
  newGVarId <- globalVarId <$> getState
  top <- topLevelScopeIdent <$> getState
  outer <- outerExtract . outerScopeIdent <$> getState
  lId <- labelId <$> getState
  jLabel <- jLabelExtract . jumpLabel <$> getState
  putState $ parseInfo {currentVarId = newVarId, globalVarId = newGVarId,
    labelId = lId, jumpLabel = jLabel, topLevelScopeIdent = top,
    outerScopeIdent = outer}

blockParser :: M.Map String IdentifierType -> ParsecT String ParseInfo IO Block
blockParser m = do
  void openCurParser
  parseInfo <- getState
  updateVarMapForNewScope parseInfo m
  modifyState $ \p -> p {topLevel = False}
  block <- manyTill blockItemParser $ try closeCurParser
  current <- currentScopeIdent <$> getState
  let outerExtract = const $ M.union (outerScopeIdent parseInfo) (M.filter isFuncIdentifier current) in
    keepIdsJumpLabel parseInfo outerExtract id
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
  keepIdsJumpLabel parseInfo id $ drop 1
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
  keepIdsJumpLabel parseInfo id $ drop 1
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
  keepIdsJumpLabel parseInfo id $ drop 1
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
            jumpLabel = preJLabel ++ SwitchLabel jLabel
              (M.insert val ("case." ++ show val ++ "." ++ show lId) caseMap) : lbs})
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
  keepIdsJumpLabel parseInfo id $ drop 1
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
isValidDataType typeName =
  M.member [typeName] . dataTypeMap <$> getState

checkForFuncNameConflict :: String -> ParsecT String ParseInfo IO String
checkForFuncNameConflict iName = do
  current <- currentScopeIdent <$> getState
  top <- topLevelScopeIdent <$> getState
  when (M.member iName current && isVarIdentifier (current M.! iName)
    || M.member iName top && isVarIdentifier (top M.! iName)) $
    unexpected "Identifier redeclared"
  pure iName

compareFunTypeDeclare :: FunTypeInfo -> FunTypeInfo -> Bool
compareFunTypeDeclare (FunTypeInfo lFt lFName _ _) (FunTypeInfo rFt rFName _ _) =
  lFt == rFt && lFName == rFName

checkForFuncTypeConflict :: ParseInfo -> Declaration -> ParsecT String ParseInfo IO ()
checkForFuncTypeConflict parseInfo declare = case declare of
  FunctionDeclaration fn _ fType _ _ sc -> do
    let current = currentScopeIdent parseInfo
    let outer = outerScopeIdent parseInfo
    let top = topLevelScopeIdent parseInfo
    let newFuncType = FunTypeInfo fType fn Nothing sc
    let checkForPrevTypeConflict m = M.member fn m
          && isFuncIdentifier (m M.! fn)
          && (not . compareFunTypeDeclare newFuncType . fti) (m M.! fn)  
    when (M.member fn current && isVarIdentifier (current M.! fn) ||
      M.member fn top && isVarIdentifier (top M.! fn)) $
      unexpected "Identifier redeclared"
    when (checkForPrevTypeConflict current || checkForPrevTypeConflict outer || checkForPrevTypeConflict top) $
      unexpected "Incompatible function type redeclare"
    modifyState (\p -> p {outerScopeIdent = M.insert fn (FuncIdentifier newFuncType) $ outerScopeIdent p})
  _ -> undefined

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

checkLocalFuncDeclare :: Maybe StorageClass -> ParsecT String ParseInfo IO (Maybe StorageClass)
checkLocalFuncDeclare sc = do
  toplvl <- topLevel <$> getState
  if not toplvl && sc == Just Static
    then unexpected "static"
    else pure sc

checkFuncStorageClass :: IdentifierName -> Maybe StorageClass -> ParsecT String ParseInfo IO ()
checkFuncStorageClass name sc = do
  top <- topLevelScopeIdent <$> getState
  when (M.member name top
    && isFuncIdentifier (top M.! name)
    && not (topLevelFuncStorageClassCheck (funcStorageClass $ fti (top M.! name)) sc)) $
    unexpected "different storage class."

updateVarMapForFuncBlockScope :: ParsecT String ParseInfo IO ()
updateVarMapForFuncBlockScope = do
  toplvl <- topLevel <$> getState
  modifyState (\p -> p {outerScopeIdent = M.union (outerScopeIdent p) (currentScopeIdent p),
    currentScopeIdent = M.empty, currentVarId = if toplvl then 1 else currentVarId p})

addFuncDelclarationIfNeed :: FunTypeInfo -> ParsecT String ParseInfo IO ()
addFuncDelclarationIfNeed newTypeInfo@(FunTypeInfo _ name _ _) = do
  parseInfo <- getState
  unless (M.member name (outerScopeIdent parseInfo) || M.member name (topLevelScopeIdent parseInfo)) $
    modifyState (\p -> p {outerScopeIdent = M.insert name (FuncIdentifier newTypeInfo) (outerScopeIdent p),
      topLevelScopeIdent = M.insert name (FuncIdentifier newTypeInfo) (topLevelScopeIdent p)})

withFunctionBody :: IdentifierType -> Bool
withFunctionBody typeIdentifier = case typeIdentifier of
  FuncIdentifier (FunTypeInfo _ _ (Just _) _) -> True
  _ -> False

checkFuncDefinition :: IdentifierName -> ParseInfo -> ParsecT String ParseInfo IO ()
checkFuncDefinition name parseInfo = do
  let current = currentScopeIdent parseInfo
      toplvl = topLevel parseInfo
  unless toplvl $ unexpected "Function definition"
  when (M.member name current && withFunctionBody (current M.! name)) $
    unexpected "Function redefinition"

updateFuncInfoIfNeed :: IdentifierName -> IdentifierType -> ParsecT String ParseInfo IO ()
updateFuncInfoIfNeed name newFuncTypeInfo  = do
  s <- getState
  unless (funcNotYetDefined name s) $
    modifyState (\p -> p {
      currentScopeIdent = M.insert name newFuncTypeInfo (currentScopeIdent p),
      outerScopeIdent = M.insert name newFuncTypeInfo (outerScopeIdent p),
      topLevelScopeIdent = M.insert name newFuncTypeInfo (topLevelScopeIdent p)}) 

functionDeclareParser :: ParsecT String ParseInfo IO Declaration
functionDeclareParser = do
  (sc, rType) <- declarationSpecifierParser True ([], [])
  name <- identifierParser >>= checkForFuncNameConflict
  checkFuncStorageClass name sc
  parseInfo <- getState
  updateVarMapForFuncBlockScope
  toplvl <- topLevel <$> getState
  aList <- between openPParser closePParser (try argListParser)
  checkForFuncTypeConflict parseInfo $
      FunctionDeclaration name aList (DTFuncType (map dataType aList) rType) Nothing 1 sc
  addFuncDelclarationIfNeed $ FunTypeInfo (DTFuncType (map dataType aList) rType) name Nothing sc
  maybeSemiColon <- lookAhead (try semiColParser <|> try openCurParser)
  block <- case maybeSemiColon of
    ";" -> semiColParser >> pure Nothing
    "{" -> checkFuncDefinition name parseInfo >>
      Just <$> blockParser (M.fromList (map (\(ArgPair dt vName) ->
        (vName, VarIdentifier dt vName toplvl Nothing sc)) aList))
    _ -> unexpected maybeSemiColon
  nVarId <- currentVarId <$> getState
  current <- currentScopeIdent <$> getState
  outer <- outerScopeIdent <$> getState
  let outerExtract = const $ M.union outer (M.filter isFuncIdentifier current) in
    keepIdsJumpLabel parseInfo outerExtract id
  topScope <- topLevelScopeIdent <$> getState
  let newSc = if M.member name topScope && isFuncIdentifier (topScope M.! name) &&
                funcStorageClass (fti (topScope M.! name)) /= Just Extern
      then funcStorageClass $ fti $ topScope M.! name
      else sc
  updateFuncInfoIfNeed name $ FuncIdentifier $ FunTypeInfo (DTFuncType (map dataType aList) rType) name block newSc
  pure $ FunctionDeclaration name aList (DTFuncType (map dataType aList) rType) block nVarId newSc

declareParser :: ParsecT String ParseInfo IO Declaration
declareParser = do
  maybeParen <-
    lookAhead (try $ many1 (try symbolExtract) >> openPParser) <|> pure ""
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
updateLabelSingle (FunctionDeclaration n args ft (Just (Block bls)) lid sc) m =
  FunctionDeclaration n args ft (Just (Block (map (`updateLabelBlockItem` m) bls))) lid sc

updateGotoLabel :: [Declaration] -> [M.Map String String] -> [Declaration]
updateGotoLabel = zipWith updateLabelSingle
