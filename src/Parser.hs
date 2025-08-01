-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Parser.hs                                          :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/03/06 12:45:56 by mayeung           #+#    #+#             --
--   Updated: 2025/07/29 13:09:25 by mayeung          ###   ########.fr       --
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
import Data.Word

type CProgramAST = [Declaration]

type VarType = String

type IdentifierName = String

data JumpLabel =
  LoopLabel (String, String, String)
  | SwitchLabel (Maybe String, String) (M.Map Integer String)
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
  deriving Eq

data DT =
  DTInternal PrimType
  | DTFuncType {argList :: [(DT, IdentifierName)], retType :: DT}
  | DTPointer {ptrPointingType :: DT}
  | DTUserDefined String [DT]

data IdentifierType =
  VarIdentifier {vti :: VarTypeInfo}
  | FuncIdentifier {fti :: FuncTypeInfo}
  deriving (Show, Eq)

data Declaration =
  VariableDeclaration VarTypeInfo
  | FunctionDeclaration FuncTypeInfo
  deriving (Show, Eq)

data VarTypeInfo =
  VarTypeInfo {varName :: IdentifierName, variableType :: DT,
    varDefine :: Maybe TypedExpr, varStoreClass :: Maybe StorageClass, topLv :: Bool}
  deriving (Show, Eq)

data FuncTypeInfo =
  FuncTypeInfo {funcName :: IdentifierName, funcType :: DT,
    funcDefine :: Maybe Block, funcStorageClass :: Maybe StorageClass, nextVarId :: Int}
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
    topLevel :: Bool,
    funcReturnType :: DT,
    switchValSize :: DT
  }
  deriving (Show, Eq)

data ForInit =
  InitDecl VarTypeInfo
  | InitExpr (Maybe TypedExpr)
  deriving (Show, Eq)

data Statement =
  Expression TypedExpr
  | Return TypedExpr
  | If TypedExpr Statement (Maybe Statement)
  | Goto String
  | Label String String Statement
  | Compound Block
  | Break String
  | Continue String
  | While TypedExpr Statement JumpLabel
  | DoWhile Statement TypedExpr JumpLabel
  | For ForInit (Maybe TypedExpr) (Maybe TypedExpr) Statement JumpLabel
  | Switch TypedExpr Statement JumpLabel
  | Case Statement String
  | Default Statement String
  | Null
  deriving (Show, Eq)

data BlockItem =
  S Statement
  | D Declaration
  deriving (Show, Eq)

newtype Block = Block {unBlock :: [BlockItem]}
  deriving (Show, Eq)

data TypedExpr = TExpr {texpr :: Expr, tDT :: DT}
  deriving (Show, Eq)

data Expr =
  Constant NumConst
  | Variable String Bool (Maybe StorageClass)
  | Cast DT TypedExpr
  | FunctionCall String [TypedExpr]
  | Unary UnaryOp TypedExpr
  | Binary BinaryOp TypedExpr TypedExpr
  | Assignment CompoundAssignOp TypedExpr TypedExpr
  | Conditional TypedExpr TypedExpr TypedExpr
  | Dereference TypedExpr
  | AddrOf TypedExpr
  deriving (Show, Eq)

data StorageClass =
  Static
  | Extern
  deriving Eq

data NumConst =
  ConstShort Integer
  | ConstUShort Integer
  | ConstInt Integer
  | ConstUInt Integer
  | ConstLong Integer
  | ConstULong Integer
  | ConstDouble Double
  deriving (Show, Eq)

instance Eq DT where
 (DTInternal l) == (DTInternal r) = l == r
 (DTFuncType lArgList lRetType) == (DTFuncType rArgList rRetType) =
    map fst lArgList == map fst rArgList && lRetType == rRetType
 (DTPointer l) == (DTPointer r) = l == r
 (DTUserDefined lName lDT) == (DTUserDefined rName rDT) = lName == rName && lDT == rDT
 _ == _ = False

instance Show PrimType where
  show TVoid = "void"
  show TChar = "char"
  show TUChar = "unsigned char"
  show TShort = "short"
  show TUShort = "unsigned short"
  show TInt = "int"
  show TUInt = "unsigned int"
  show TLong = "long"
  show TULong = "unsigned long"
  show TFloat = "float"
  show TDouble = "double"
  show TLDouble = "long double"

instance Show DT where
  show (DTInternal pt) = show pt
  show (DTPointer t) = show t ++ " *"
  show (DTFuncType aList rType) = show rType ++ " " ++ "(" ++ show aList ++ ")"
  show (DTUserDefined name dts) = name ++ " " ++ show dts

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
  ["!", "~", "--", "++", "-", "+", "*", "&"]

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
    ["short"], ["signed", "short"], ["short", "int"], ["signed", "short", "int"],
    ["unsigned", "short"], ["unsigned", "short", "int"],
    ["int"], ["signed", "int"], ["signed"],
    ["unsigned", "int"], ["unsigned"],
    ["long"], ["long", "int"], ["signed", "long"], ["signed", "long", "int"],
    ["long", "long"], ["long", "long", "int"], ["signed", "long", "long"], 
    ["signed", "long", "long", "int"],
    ["unsigned", "long"], ["unsigned", "long", "long"],
    ["unsigned", "long", "int"], ["unsigned", "long", "long", "int"],
    ["float"], ["double"], ["long", "double"]]

primTypeList :: [PrimType]
primTypeList =
  [TVoid, TChar, TChar, TUChar,
    TShort, TShort, TShort, TShort,
    TUShort, TUShort,
    TInt, TInt, TInt,
    TUInt, TUInt,
    TLong, TLong, TLong, TLong,
    TLong, TLong, TLong, TLong,
    TULong, TULong,
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
    topLevel = True,
    funcReturnType = DTInternal TVoid,
    switchValSize = DTInternal TInt
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

dotLex :: ParsecT String u IO Char
dotLex = char '.'

eLex :: ParsecT String u IO Char
eLex = oneOf "eE"

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
  specifier <- lookAhead $ optionMaybe $ try symbolExtract
  when (isJust specifier && fromJust specifier `elem` declarationSpecifierStrList
    && fromJust specifier `elem` prohibitedList) $
    unexpected $ fromJust specifier
  toplvl <- topLevel <$> getState
  when (isFunc && not toplvl && specifier == Just "static") $
    unexpected $ fromJust specifier
  if isNothing specifier || isJust specifier && fromJust specifier `notElem` declarationSpecifierStrList
    then do
      unless (any (`notElem` storageClassSpecifierStrList) toks) $
        unexpected "identifier. Need type specifier"
      let sc = case filter (`elem` storageClassSpecifierStrList) toks of
            [] -> Nothing 
            ["static"] -> Just Static
            ["extern"] -> Just Extern
            _ -> undefined
      let varType = primDataTypeMap M.! sort (filter (`notElem` storageClassSpecifierStrList) toks)
      pure (sc, varType)
    else
      let newPBList =
           (if (== 2) $ length $ filter (== "long") (fromJust specifier : toks)
            then ["void", "char", "short", "long", "float", "double"]
            else []) ++
            concatMap prohibitedNextTokensList (fromJust specifier : toks) in
      symbolExtract >> declarationSpecifierParser isFunc (fromJust specifier : toks, newPBList)

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
  "static" -> storageClassSpecifierStrList
  "extern" -> storageClassSpecifierStrList
  _ -> []

typeSpecifierParser :: ([String], [String]) -> ParsecT String ParseInfo IO DT
typeSpecifierParser (toks, prohibitedList) = do
  specifier <- lookAhead symbolExtract
  when (specifier `elem` declarationSpecifierStrList
    && specifier `elem` prohibitedList) $
    unexpected specifier
  when (specifier `elem` storageClassSpecifierStrList) $
    unexpected specifier
  if specifier `notElem` declarationSpecifierStrList
    then do
      when (null toks) $
        unexpected "identifier. Need type specifier"
      pure $ primDataTypeMap M.! sort (filter (`notElem` storageClassSpecifierStrList) toks)
    else
      let newPBList =
           (if (== 2) $ length $ filter (== "long") (specifier : toks)
            then ["void", "char", "short", "long", "float", "double"]
            else []) ++
            concatMap prohibitedNextTokensList (specifier : toks) in
      symbolExtract >> typeSpecifierParser (specifier : toks, newPBList)

dataTypeParser :: ParsecT String ParseInfo IO DT
dataTypeParser = do
  dtName <- symbolExtract
  dtMap <- dataTypeMap <$> getState
  if M.member [dtName] dtMap && dtName /= "void"
    then pure $ dtMap M.! [dtName]
    else unexpected dtName

significandParser :: ParsecT String u IO String
significandParser = do
  spaces
  withLeadingDigit <|> withoutLeadingDigit
  where beforeDotDigit = many1 digit
        aferDotDigit = do
          ds <- many digit
          if null ds
            then pure "0"
            else pure ds
        dotAndDigit = (++) . pure <$> dotLex <*> aferDotDigit <|> pure ""
        withLeadingDigit = (++) <$> beforeDotDigit <*> dotAndDigit
        withoutLeadingDigit = (++) . ("0" ++) . pure <$> dotLex <*> many1 digit

exponentParser :: ParsecT String u IO String
exponentParser = do
  e <- pure <$> oneOf "eE"
  sign <- pure <$> oneOf "+-" <|> pure ""
  eVal <- many1 digit
  pure $ e ++ sign ++ eVal

doubleParser :: ParsecT String u IO String
doubleParser = do
  spaces
  significandStr <- significandParser
  exponentStr <- exponentParser <|> pure ""
  pure $ significandStr ++ exponentStr

intParser :: ParsecT String u IO String
intParser = spaces >> many1 digit

uintParser :: ParsecT String u IO String
uintParser = spaces >> many1 digit <* oneOf "uU" 

longParser :: ParsecT String u IO String
longParser = spaces >> many1 digit <* oneOf "lL"

ulongParser :: ParsecT String u IO String
ulongParser = spaces >> many1 digit <* (try (oneOf "uU" >> oneOf "lL") <|> (oneOf "lL" >> oneOf "uU"))

integerParser :: ParsecT String u IO String
integerParser = try ulongParser <|> try longParser <|> try uintParser <|> intParser

numParser :: ParsecT String u IO String
numParser = try doubleParser <|> integerParser

fileParser :: ParsecT String ParseInfo IO (M.Map String IdentifierType, [Declaration])
fileParser = do
  declares <- manyTill declareParser $ try $ spaces >> eof
  top <- topLevelScopeIdent <$> getState
  pure (top, declares)

binaryAssignmentOpParser :: ParsecT String u IO CompoundAssignOp
binaryAssignmentOpParser = foldl1 (<|>) $
  map try $
    zipWith (>>)
      [plusAssignLex, minusAssignLex, multiAssignLex, divAssignLex,
        modAssignLex, bitAndAssignLex, bitOrAssignLex, bitXorAssignLex,
        bitLeftShiftAssignLex, bitRightShiftAssignLex, assignmentLex] $
      map pure
        [PlusAssign, MinusAssign, MultiplyAssign, DivisionAssign,
          ModuloAssign, BitAndAssign, BitOrAssign, BitXorAssign,
          BitShiftLeftAssign, BitShiftRightAssign, AssignOp]

compoundAssignOpToBinOp :: CompoundAssignOp -> BinaryOp
compoundAssignOpToBinOp op = (M.! op) $ M.fromList $ zip
  [BitAndAssign, BitOrAssign, BitXorAssign, BitShiftLeftAssign, BitShiftRightAssign, PlusAssign,
    MinusAssign, MultiplyAssign, DivisionAssign, ModuloAssign, AssignOp]
  [BitAnd, BitOr, BitXor, BitShiftLeft, BitShiftRight, Plus,
    Minus, Multiply, Division, Modulo, None]

unsupportedFloatOperation :: [BinaryOp]
unsupportedFloatOperation =
  [Modulo, BitAnd, BitOr, BitXor,
    BitShiftLeft, BitShiftRight]

unsupportedPointerOperation :: [BinaryOp]
unsupportedPointerOperation =
  [Multiply, Division, Modulo, BitAnd, BitOr, BitXor,
    BitShiftLeft, BitShiftRight]

unsupportedPointerUnaryOperation :: [UnaryOp]
unsupportedPointerUnaryOperation =
  [Complement,
    Negate,
    UPlus]

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

relationOp :: [BinaryOp]
relationOp = [EqualRelation, NotEqualRelation,
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

incrementDecrement :: [UnaryOp]
incrementDecrement = [PreIncrement, PreDecrement, PostIncrement, PostDecrement]

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
      exclaimLex, complementLex, bitAndLex, mulLex]

exprToInteger :: TypedExpr -> Integer
exprToInteger (TExpr expr _) = case expr of
  Constant (ConstInt i) -> i
  Constant (ConstUInt ui) -> ui
  Constant (ConstLong l) -> l
  Constant (ConstULong ul) -> ul
  Constant (ConstDouble d) -> truncate d
  _ -> undefined

exprToDouble :: TypedExpr -> Double
exprToDouble (TExpr expr _) = case expr of
  Constant (ConstInt i) -> fromIntegral i
  Constant (ConstUInt ui) -> fromIntegral ui
  Constant (ConstLong l) -> fromIntegral l
  Constant (ConstULong ul) -> fromIntegral ul
  Constant (ConstDouble d) -> d
  _ -> undefined

numConstToInt :: Num b => NumConst -> b
numConstToInt n = case n of
  ConstShort s -> fromIntegral s
  ConstUShort us -> fromIntegral us
  ConstInt i -> fromIntegral i
  ConstLong l -> fromIntegral l
  ConstUInt ui -> fromIntegral ui
  ConstULong ul -> fromIntegral ul
  _ -> undefined

numConstToDouble :: NumConst -> Double
numConstToDouble n = case n of
  ConstDouble d -> d
  _ -> undefined

numConstToStr :: NumConst -> String
numConstToStr n = case n of
  ConstShort s -> show s
  ConstUShort us -> show us
  ConstInt i -> show i
  ConstLong l -> show l
  ConstUInt ui -> show ui
  ConstULong ul -> show ul
  ConstDouble d -> show d

isVariableExpr :: TypedExpr -> Bool
isVariableExpr (TExpr (Variable {}) _) = True
isVariableExpr _ = False

isLValueExpr :: TypedExpr -> Bool
isLValueExpr (TExpr (Variable {}) _) = True
isLValueExpr (TExpr (Dereference _) _) = True
isLValueExpr _ = False

isConstantTypedExpr :: TypedExpr -> Bool
isConstantTypedExpr (TExpr expr _) = case expr of
  Constant _ -> True
  Variable {} -> False
  Cast _ e -> isConstantTypedExpr e
  FunctionCall {} -> False
  Unary op e -> (op `notElem` incrementDecrement) && isConstantTypedExpr e
  Binary _ l r -> isConstantTypedExpr l && isConstantTypedExpr r
  Assignment {} -> False
  Conditional c t f -> isConstantTypedExpr c && isConstantTypedExpr t && isConstantTypedExpr f
  AddrOf {} -> False
  Dereference {} -> False

isIntegralConstantTE :: TypedExpr -> Bool
isIntegralConstantTE (TExpr expr _) = case expr of
  Constant (ConstShort _) -> True
  Constant (ConstUShort _) -> True
  Constant (ConstInt _) -> True
  Constant (ConstUInt _) -> True
  Constant (ConstLong _) -> True
  Constant (ConstULong _) -> True
  _ -> False

isFloatConstantTE :: TypedExpr -> Bool
isFloatConstantTE (TExpr expr _) = case expr of
  Constant (ConstDouble _) -> True
  _ -> False

isFloatTypedExpr :: TypedExpr -> Bool
isFloatTypedExpr = (== DTInternal TDouble) . tDT

isIntegralTypedExpr :: TypedExpr -> Bool
isIntegralTypedExpr = isIntDT . tDT

isPointerTypedExpr :: TypedExpr -> Bool
isPointerTypedExpr (TExpr _ (DTPointer _)) = True
isPointerTypedExpr _ = False

isFloatDT :: DT -> Bool
isFloatDT dt = case dt of
  DTInternal TDouble -> True
  _ -> False

exprToConstantExpr :: TypedExpr -> TypedExpr
exprToConstantExpr te@(TExpr e dt) =
  let constructor = case dt of
        DTInternal TInt -> Constant . ConstInt . toInteger . (fromInteger :: Integer -> Int32)
        DTInternal TUInt -> Constant . ConstUInt . toInteger . (fromInteger :: Integer -> Word32) 
        DTInternal TLong -> Constant . ConstLong . toInteger . (fromInteger :: Integer -> Int64)
        DTInternal TULong -> Constant . ConstULong . toInteger . (fromInteger :: Integer -> Word64)
        DTPointer _ -> Constant . ConstULong . toInteger . (fromInteger :: Integer -> Word64)
        _ -> undefined
        in
  case e of
  Constant _ -> te
  Cast cDt expr -> if cDt == DTInternal TDouble
    then (`TExpr` dt) $ Constant $ ConstDouble $ exprToDouble $ exprToConstantExpr expr
    else (`TExpr` dt) $ constructor $ exprToInteger $ exprToConstantExpr expr
  Unary op expr -> if isFloatDT $ tDT expr
    then (`TExpr` dt) $ Constant $ ConstDouble $ unaryOpToHaskellOperatorDouble op $ exprToDouble $ exprToConstantExpr expr
    else (`TExpr` dt) $ constructor $ unaryOpToHaskellOperator op $ exprToInteger $ exprToConstantExpr expr
  Binary op lExpr rExpr -> case op of
    LogicAnd -> relationToIntConst (&&) lExpr rExpr
    LogicOr -> relationToIntConst (||) lExpr rExpr
    EqualRelation -> if isFloatDT $ tDT lExpr
      then floatComparsionToIntConst (==) lExpr rExpr
      else intComparsionToIntConst (==) lExpr rExpr
    NotEqualRelation -> if isFloatDT $ tDT lExpr
      then floatComparsionToIntConst (/=) lExpr rExpr
      else intComparsionToIntConst (/=) lExpr rExpr
    GreaterEqualRelation -> if isFloatDT $ tDT lExpr
      then floatComparsionToIntConst (>=) lExpr rExpr
      else intComparsionToIntConst (>=) lExpr rExpr
    GreaterThanRelation ->  if isFloatDT $ tDT lExpr
      then floatComparsionToIntConst (>) lExpr rExpr
      else intComparsionToIntConst (>) lExpr rExpr
    LessEqualRelation -> if isFloatDT $ tDT lExpr
      then floatComparsionToIntConst (<=) lExpr rExpr
      else intComparsionToIntConst (<=) lExpr rExpr
    LessThanRelation -> if isFloatDT $ tDT lExpr
      then floatComparsionToIntConst (<) lExpr rExpr
      else intComparsionToIntConst (<) lExpr rExpr
    _ ->
      if isFloatDT $ tDT lExpr
        then (`TExpr` dt) $ Constant $ ConstDouble $ binaryOpToHaskellOperatorDouble op
              (exprToDouble $ exprToConstantExpr lExpr)
              (exprToDouble $ exprToConstantExpr rExpr)
        else (`TExpr` dt) $ constructor $ binaryOpToHaskellOperator op
              (exprToInteger $ exprToConstantExpr lExpr)
              (exprToInteger $ exprToConstantExpr rExpr)
  Conditional c t f -> if isFloatDT $ tDT t
    then (`TExpr` dt) $ Constant $ ConstDouble $
      if isFloatDT $ tDT c
        then if exprToDouble (exprToConstantExpr c) /= 0.0 then
                  exprToDouble $ exprToConstantExpr t else exprToDouble $ exprToConstantExpr f
        else if exprToInteger (exprToConstantExpr c) /= 0 then
                  exprToDouble $ exprToConstantExpr t else exprToDouble $ exprToConstantExpr f
    else (`TExpr` dt) $ constructor $ 
      if exprToInteger (exprToConstantExpr c) /= 0 then
          exprToInteger $ exprToConstantExpr t else exprToInteger $ exprToConstantExpr f
  _ -> undefined
  where relationToIntConst op lE rE =
          let lVal = if isFloatDT $ tDT lE then (/= 0.0) $ exprToDouble lE else (/= 0) $ exprToInteger lE
              rVal = if isFloatDT $ tDT rE then (/= 0.0) $ exprToDouble rE else (/= 0) $ exprToInteger rE in
          if lVal `op` rVal
            then makeConstantTEIntWithDT 1 $ DTInternal TInt
            else makeConstantTEIntWithDT 0 $ DTInternal TInt
        intComparsionToIntConst op lE rE =
          if exprToInteger lE `op` exprToInteger rE
            then makeConstantTEIntWithDT 1 $ DTInternal TInt
            else makeConstantTEIntWithDT 0 $ DTInternal TInt
        floatComparsionToIntConst op lE rE =
          if exprToDouble lE `op` exprToDouble rE
            then makeConstantTEIntWithDT 1 $ DTInternal TInt
            else makeConstantTEIntWithDT 0 $ DTInternal TInt


foldToConstExpr :: TypedExpr -> TypedExpr
-- foldToConstExpr = id
foldToConstExpr te = if isConstantTypedExpr te then exprToConstantExpr te else te

doubleIntegralParser :: ParsecT String ParseInfo IO TypedExpr
doubleIntegralParser = do
  maybeDotE <- lookAhead $ optionMaybe $ try $ spaces >> (dotLex <|> (many1 digit >> (dotLex <|> eLex)))
  if isJust maybeDotE
    then do
      d <- doubleParser
      let dVal = read d :: Double
      pure $ TExpr (Constant (ConstDouble dVal)) $ DTInternal TDouble
    else
      intLongUnsignedConstantParser

intLongUnsignedConstantParser :: ParsecT String ParseInfo IO TypedExpr
intLongUnsignedConstantParser = do
  maybeULong <- lookAhead $ optionMaybe $ try ulongParser
  case maybeULong of
    Just l -> do
      void ulongParser
      let lVal = read l :: Integer
      if lVal <= fromIntegral (maxBound :: Word64)
        then pure $ TExpr (Constant $ ConstULong $ fromInteger lVal) $ DTInternal TULong
        else unexpected "large unsigned integer value"
    _ -> do
      maybeUInt <- lookAhead $ optionMaybe $ try uintParser
      case maybeUInt of
        Just ui -> do
            void uintParser
            let uiVal = read ui :: Integer
            if uiVal <= fromIntegral (maxBound :: Word32)
              then pure $ TExpr (Constant $ ConstUInt $ fromIntegral uiVal) $ DTInternal TUInt
              else if uiVal <= fromIntegral (maxBound :: Word64)
                then pure $ TExpr (Constant $ ConstULong $ fromIntegral uiVal) $ DTInternal TULong
                else unexpected "large unsigned integer value"
        _ -> intLongConstantParser

intLongConstantParser :: ParsecT String ParseInfo IO TypedExpr
intLongConstantParser = do
  maybeLong <- lookAhead $ optionMaybe $ try longParser
  case maybeLong of
    Just l -> do
      void longParser
      let lVal = read l :: Integer
      if lVal <= fromIntegral (maxBound :: Int64)
        then pure $ TExpr (Constant $ ConstLong $ fromInteger lVal) $ DTInternal TLong
        else unexpected "large integer value"
    _ -> do
      val <- intParser
      let iVal = read val :: Integer
      if iVal <= fromIntegral (maxBound :: Int32)
        then pure $ TExpr (Constant $ ConstInt $ fromIntegral iVal) $ DTInternal TInt
        else if iVal <= fromIntegral (maxBound :: Int64)
          then pure $ TExpr (Constant $ ConstLong $ fromIntegral iVal) $ DTInternal TLong
          else unexpected "large integer value"

cvtTypedExpr :: TypedExpr -> DT -> TypedExpr
cvtTypedExpr te@(TExpr _ dt) cDT
  | dt == cDT = te
  | otherwise = TExpr (Cast cDT te) cDT

getPointingType :: DT -> DT
getPointingType dt = case dt of
  DTPointer pDT -> pDT
  _ -> dt

derefParser :: ParsecT String ParseInfo IO TypedExpr
derefParser = do
  void mulLex
  p <- precedence <$> getState
  modifyState $ setPrecedence $ getUnaryOpPrecedence "*"
  expr <- exprParser
  unless (isPointerDT $ tDT expr) $
    unexpected "dereference of non pointer type"
  modifyState $ setPrecedence p
  pure $ TExpr (Dereference expr) $ getPointingType $ tDT expr

addrOfParser :: ParsecT String ParseInfo IO TypedExpr
addrOfParser = do
  void bitAndLex
  p <- precedence <$> getState
  modifyState $ setPrecedence $ getUnaryOpPrecedence "&"
  expr <- exprParser
  unless (isLValueExpr expr) $
    unexpected "non lvalue"
  modifyState $ setPrecedence p
  pure $ TExpr (AddrOf expr) $ DTPointer $ tDT expr

unaryExprParser :: ParsecT String ParseInfo IO TypedExpr
unaryExprParser = do
  uOpStr <- lookAhead unaryOpStringParser
  case uOpStr of
    "*" -> derefParser
    "&" -> addrOfParser
    _ -> do
      uOp <- unaryOpParser
      p <- precedence <$> getState
      modifyState $ setPrecedence $ getUnaryOpPrecedence uOpStr
      expr <- exprParser
      when (uOp == Complement && isFloatTypedExpr expr) $
        unexpected "Complement unary operation for floating point number"
      when (uOp `elem` [PreDecrement, PreIncrement] && not (isLValueExpr expr)) $
        unexpected "Need lvalue for prefix operation"
      when (uOp `elem` unsupportedPointerUnaryOperation && isPointerTypedExpr expr) $
        unexpected "Invalid unary operation for pointer"
      let dt = if uOp == NotRelation then DTInternal TInt else tDT expr
      modifyState $ setPrecedence p
      pure $ foldToConstExpr (TExpr (Unary uOp expr) dt)

makeConstantTEIntWithDT :: Integer -> DT -> TypedExpr
makeConstantTEIntWithDT n dt = case dt of
  DTInternal TShort -> TExpr (Constant $ ConstShort n) dt
  DTInternal TUShort -> TExpr (Constant $ ConstUShort n) dt
  DTInternal TInt -> TExpr (Constant $ ConstInt n) dt
  DTInternal TUInt -> TExpr (Constant $ ConstUInt n) dt
  DTInternal TLong -> TExpr (Constant $ ConstLong n) dt
  DTInternal TULong -> TExpr (Constant $ ConstULong n) dt
  _ -> undefined

makeConstantTEFloatWithDT :: Double -> DT -> TypedExpr
makeConstantTEFloatWithDT n dt = case dt of
  DTInternal TDouble -> TExpr (Constant $ ConstDouble n) dt
  _ -> undefined

getDTSize :: DT -> Int
getDTSize dt = case dt of
  DTInternal TChar -> 1
  DTInternal TUChar -> 1
  DTInternal TShort -> 2
  DTInternal TUShort -> 2
  DTInternal TInt -> 4
  DTInternal TUInt -> 4
  DTInternal TLong -> 8
  DTInternal TULong -> 8
  DTInternal TFloat -> 4
  DTInternal TDouble -> 8
  DTInternal TLDouble -> 16
  DTPointer _ -> 8
  _ -> undefined

isSignedInteger :: DT -> Bool
isSignedInteger dt = case dt of
  DTInternal TChar -> True
  DTInternal TShort -> True
  DTInternal TInt -> True
  DTInternal TLong -> True
  _ -> False

isUnsigned :: DT -> Bool
isUnsigned dt = case dt of
  DTInternal TUChar -> True
  DTInternal TUShort -> True
  DTInternal TUInt -> True
  DTInternal TULong -> True
  DTPointer _ -> True
  _ -> False

isFloatConstNumConst :: NumConst -> Bool
isFloatConstNumConst c = case c of
  ConstDouble _ -> True
  _ -> False

isIntegralConstantNumConst :: NumConst -> Bool
isIntegralConstantNumConst c = case c of
  ConstShort _ -> True
  ConstUShort _ -> True
  ConstInt _ -> True
  ConstUInt _ -> True
  ConstLong _ -> True
  ConstULong _ -> True
  ConstDouble _ -> False

getExprsCommonType :: TypedExpr -> TypedExpr -> DT
getExprsCommonType (TExpr _ lDT) (TExpr _ rDT)
  | lDT == rDT =lDT
  | lDT == DTInternal TDouble || rDT == DTInternal TDouble = DTInternal TDouble
  | isSameSize lDT rDT && isSignedInteger lDT = rDT
  | isSameSize lDT rDT && isSignedInteger rDT = lDT
  | getDTSize lDT > getDTSize rDT = lDT
  | otherwise = rDT
  where isSameSize l r = getDTSize l == getDTSize r

exprParser :: ParsecT String ParseInfo IO TypedExpr
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

conditionalTrueParser :: ParsecT String ParseInfo IO TypedExpr
conditionalTrueParser = do
  void questionParser
  oldState <- precedence <$> getState
  modifyState $ updatePrecedence lowestPrecedence
  expr <- exprParser
  modifyState $ setPrecedence oldState
  void colonParser
  pure $ foldToConstExpr expr

compoundAssignParser :: TypedExpr -> String -> ParsecT String ParseInfo IO TypedExpr
compoundAssignParser l@(TExpr lExpr lDt) binOp = do
  let varDerefPtr =
        do
          op <- binaryAssignmentOpParser
          modifyState $ setPrecedence $ getBinOpPrecedence binOp
          e <- exprParser
          when ((compoundAssignOpToBinOp op `elem` unsupportedFloatOperation) && (isFloatTypedExpr l || isFloatTypedExpr e)) $
            unexpected "binary operation for floating point number"
          when (isPointerTypedExpr l
            && not (isPointerTypedExpr e || (isIntegralConstantTE e && exprToInteger e == 0))) $
            unexpected "assignment operation for pointer"
          when (isPointerTypedExpr l
            && (compoundAssignOpToBinOp op `elem` unsupportedPointerOperation)) $
            unexpected "compound assignment operation for pointer"
          checkImplicitCast e lDt
          exprRightParser $ TExpr (Assignment op l e) lDt
  case lExpr of
        Variable {} -> varDerefPtr
        Dereference {} -> varDerefPtr
        _ -> unexpected "Invalid lvalue on the left side"

conditionalParser :: TypedExpr -> String -> ParsecT String ParseInfo IO TypedExpr
conditionalParser l binOp = do
  p <- precedence <$> getState
  tCond <- conditionalTrueParser
  modifyState $ setPrecedence $ getBinOpPrecedence binOp
  fCond <- exprParser
  let cType = getExprsCommonType tCond fCond
  modifyState $ setPrecedence p
  exprRightParser $ (`TExpr` cType) $
    Conditional l
      (if isPointerTypedExpr tCond then tCond else foldToConstExpr $ cvtTypedExpr tCond cType)
      (if isPointerTypedExpr fCond then fCond else foldToConstExpr $ cvtTypedExpr fCond cType)

binaryParser :: TypedExpr -> String -> ParsecT String ParseInfo IO TypedExpr
binaryParser l binOp = do
  p <- precedence <$> getState
  op <- binaryOpParser
  modifyState $ updatePrecedence $ getBinOpPrecedence binOp
  rExpr <- exprParser
  when (op `elem` unsupportedFloatOperation && (isFloatTypedExpr l || isFloatTypedExpr rExpr)) $
    unexpected "binary operation for floating point number"
  when (op `elem` unsupportedPointerOperation && (isPointerTypedExpr l || isPointerTypedExpr rExpr)) $
      unexpected "binary operation for pointer"
  unless (op `elem` [LogicAnd, LogicOr]) $
    checkImplicitCast rExpr $ tDT l
  modifyState $ setPrecedence p
  case op of
    LogicAnd -> exprRightParser $ TExpr (Binary op (foldToConstExpr l) (foldToConstExpr rExpr)) $ DTInternal TInt
    LogicOr -> exprRightParser $ TExpr (Binary op (foldToConstExpr l) (foldToConstExpr rExpr)) $ DTInternal TInt
    _ -> do
      let cType = getExprsCommonType l rExpr
      let dt
            | op `elem` [Plus, Minus, Multiply, Division, Modulo, BitAnd, BitOr, BitXor] = cType
            | op `elem` [BitShiftLeft, BitShiftRight] = tDT l
            | otherwise = DTInternal TInt
      exprRightParser $ (`cvtTypedExpr` dt) $ TExpr
        (Binary op (foldToConstExpr (cvtTypedExpr l cType)) (foldToConstExpr (cvtTypedExpr rExpr cType))) dt

exprRightParser :: TypedExpr -> ParsecT String ParseInfo IO TypedExpr
exprRightParser l = do
  binOp <- lookAhead (try binaryOpStringParser) <|> pure ""
  p <- precedence <$> getState
  if isBinaryOpChar binOp && isEqOrHigherPrecedence binOp p
    then if binOp `elem` binaryAssignmentOp
          then compoundAssignParser l binOp
          else if binOp == "?"
            then conditionalParser l binOp
            else binaryParser l binOp
    else pure l

intOperandParser :: ParsecT String u IO TypedExpr
intOperandParser = flip TExpr (DTInternal TInt) . Constant . ConstInt . read <$> intParser

longOperandParser :: ParsecT String u IO TypedExpr
longOperandParser = flip TExpr (DTInternal TLong) . Constant . ConstLong . read <$> longParser

postfixOp :: TypedExpr -> ParsecT String u IO TypedExpr
postfixOp e@(TExpr _ dt) =
  if isLValueExpr e
    then do
      maybePostOp <- lookAhead (try $ incrementLex <|> decrementLex) <|> pure ""
      if maybePostOp `elem` allPostUnaryOp
        then do
          postUOp <- postUnaryOpParser
          pure $ (`TExpr` dt) $ Unary postUOp e
        else pure e
    else pure e

parenExprParser :: ParsecT String ParseInfo IO TypedExpr
parenExprParser = do
  void openPParser
  p <- precedence <$> getState
  modifyState $ updatePrecedence lowestPrecedence
  expr <- exprParser
  void closePParser
  modifyState $ setPrecedence p
  postfixOp $ foldToConstExpr expr

variableParser :: ParsecT String ParseInfo IO TypedExpr
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
    then let var = vti $ current M.! vName in
        postfixOp $ TExpr (Variable (varName var) (topLv var) (varStoreClass var)) (variableType var)
    else let var = vti $ outer M.! vName in
        postfixOp $ TExpr  (Variable (varName var) (topLv var) (varStoreClass var)) (variableType var)

isFuncIdentiferInVarMap :: String -> M.Map String IdentifierType -> M.Map String IdentifierType -> Bool
isFuncIdentiferInVarMap fIdentifier current outer =
  (M.member fIdentifier current && isFuncIdentifier (current M.! fIdentifier))
    || (M.member fIdentifier outer && isFuncIdentifier (outer M.! fIdentifier))

getFunTypeInfoFromVarMap :: String -> M.Map String IdentifierType -> M.Map String IdentifierType -> FuncTypeInfo
getFunTypeInfoFromVarMap fIdentifier current outer =
  if M.member fIdentifier current && isFuncIdentifier (current M.! fIdentifier)
    then fti $ current M.! fIdentifier
    else fti $ outer M.! fIdentifier

checkPointerInit :: TypedExpr -> DT -> Maybe StorageClass -> ParsecT String ParseInfo IO ()
checkPointerInit te dt sc = do
  when (isPointerDT dt && sc == Just Static
    && not (isConstantTypedExpr te && isIntegralTypedExpr te && exprToInteger te == 0)) $
    unexpected "non 0 init for pointer"
  when (isPointerDT dt && sc /= Just Static
    && (isFloatTypedExpr te ||
    not (isPointerTypedExpr te || (isConstantTypedExpr te && isIntegralTypedExpr te && exprToInteger te == 0)))) $
    unexpected "non 0 init for pointer"
  case te of
    TExpr (Conditional _ t f) _ ->do
      when (isPointerDT dt && sc /= Just Static
        && (isPointerTypedExpr t && ptrPointingType dt /= ptrPointingType (tDT t)
        || isPointerTypedExpr f && ptrPointingType dt /= ptrPointingType (tDT f))) $
        unexpected "implicit type cast for pointer"
    _ -> pure ()

checkImplicitCast :: TypedExpr -> DT -> ParsecT String ParseInfo IO ()
checkImplicitCast te dt = do
  when (isPointerTypedExpr te && not (isPointerDT dt)) $
    unexpected "implicit type cast for pointer"
  when (isPointerTypedExpr te && isPointerDT dt && ptrPointingType dt /= ptrPointingType (tDT te)) $
    unexpected "implicit type cast for pointer"
  when (isPointerDT dt
    && not (isPointerTypedExpr te || (isConstantTypedExpr te && isIntegralTypedExpr te && exprToInteger te == 0))) $
    unexpected "implicit type cast for pointer"
  case te of
    TExpr (Conditional _ t f) _ -> when (isPointerDT dt
      && (isPointerTypedExpr t && ptrPointingType dt /= ptrPointingType (tDT t)
      || isPointerTypedExpr f && ptrPointingType dt /= ptrPointingType (tDT f))) $
      unexpected "implicit type cast for pointer"
    _ -> pure ()

checkExplicitCast :: TypedExpr -> DT -> ParsecT String ParseInfo IO ()
checkExplicitCast te dt = do
  when (isPointerDT dt && isFloatTypedExpr te) $
    unexpected "explicit type cast for pointer"
  when (isFloatDT dt && isPointerTypedExpr te) $
    unexpected "explicit type cast for pointer"

functionCallParser :: ParsecT String ParseInfo IO TypedExpr
functionCallParser = do
  functionName <- identifierParser <* openPParser
  current <- currentScopeIdent <$> getState
  outer <- outerScopeIdent <$> getState
  when (M.member functionName current && isVarIdentifier (current M.! functionName)) $
    unexpected "using variable as function."
  unless (isFuncIdentiferInVarMap functionName current outer) $
    unexpected $ functionName ++ ". Not declared before"
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
  unless (length paraList == length (argList $ funcType funcInfo)) $
    unexpected "Function call. Incorrect number of parameters"
  mapM_ (uncurry checkImplicitCast) $ zip paraList $ map fst $ argList $ funcType funcInfo
  let convertedParaList = zipWith cvtTypedExpr paraList (map fst $ argList $ funcType funcInfo)
  pure $ TExpr
    (FunctionCall functionName (map foldToConstExpr convertedParaList))
    (retType $ funcType $ fti $ outer M.! functionName)

isAssignmentExpr :: Expr -> Bool
isAssignmentExpr e = case e of
  Assignment {} -> True
  _ -> False

castParser :: ParsecT String ParseInfo IO TypedExpr
castParser = do
  void openPParser
  (_, baseType) <- declarationSpecifierParser False ([], storageClassSpecifierStrList)
  (name, declarator) <- declaratorParser (show baseType) "" [id]
  unless (null name) $
    unexpected "declarator"
  void closePParser
  p <- precedence <$> getState
  modifyState $ setPrecedence 2
  e@(TExpr innerE _) <- exprParser
  when (isAssignmentExpr innerE) $
    unexpected "cast of assignment expression"
  let cType = foldl1 (.) declarator baseType
  when (isFuncDT cType) $
    unexpected "cast to function"
  modifyState $ setPrecedence p
  checkExplicitCast e cType
  pure $ foldToConstExpr $ TExpr (Cast cType e) cType

factorParser :: ParsecT String ParseInfo IO TypedExpr
factorParser = do
  c <- lookAhead $ try $ spaces >> anyChar
  next c where
    next c
      | isDigit c || c == '.' = doubleIntegralParser
      | [c] `elem` allUnaryOp = unaryExprParser
      | c == '(' = do
        maybeDT <- lookAhead $ optionMaybe $ try $
          openPParser >> declarationSpecifierParser False ([], storageClassSpecifierStrList)
        case maybeDT of
          Just _ -> castParser
          _ -> parenExprParser
      | otherwise = do
          maybeParen <- lookAhead (try (identifierParser >> openPParser)) <|> pure ""
          case maybeParen of
              "(" -> functionCallParser
              _ -> variableParser

returnStatParser :: ParsecT String ParseInfo IO Statement
returnStatParser = do
  void keyReturnParser
  rType <- funcReturnType <$> getState
  expr <- exprParser
  checkImplicitCast expr rType
  let cvtedTE = cvtTypedExpr expr rType
  void semiColParser
  pure $ Return $ foldToConstExpr cvtedTE

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
  pure $ If (foldToConstExpr cond) tStat fStat

argPairParser :: ParsecT String ParseInfo IO (DT, IdentifierName)
argPairParser = do
  (_, baseType) <- declarationSpecifierParser False ([], "void" : storageClassSpecifierStrList)
  (vName, declarator) <- declaratorParser (show baseType) "" [id]
  let varType = foldl1 (.) declarator baseType
  current <- currentScopeIdent <$> getState
  when (M.member vName current) $
    unexpected $ vName ++ ". Already defined"
  unless (null vName) $
    modifyState (\p -> p {currentScopeIdent =
      M.insert vName (VarIdentifier (VarTypeInfo vName varType Nothing Nothing False)) (currentScopeIdent p)})
  pure (varType, vName)

argListParser :: ParsecT String ParseInfo IO [(DT, IdentifierName)]
argListParser = do
  void openPParser
  res <- lookAhead (try closePParser <|> try symbolExtract) <|> pure ""
  r <- case res of
    "void" -> keyVoidParser >> pure []
    ")" -> pure []
    _ -> sepBy argPairParser $ try commaParser
  void closePParser
  pure r

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

updateTopLevelVarCurrentVar :: DT -> IdentifierName -> Maybe TypedExpr ->
  Maybe StorageClass -> ParsecT String ParseInfo IO ()
updateTopLevelVarCurrentVar dt vName vDf sClass = do
  topMap <- topLevelScopeIdent <$> getState
  let typeInfo = if M.member vName topMap
      then case varStoreClass $ vti $ topMap M.! vName of
        Just Extern -> VarIdentifier (VarTypeInfo vName dt vDf sClass True)
        ogSClass -> case varDefine $ vti $ topMap M.! vName of
          og@(Just _) -> VarIdentifier (VarTypeInfo vName dt og ogSClass True)
          _ -> VarIdentifier (VarTypeInfo vName dt vDf ogSClass True)
      else VarIdentifier (VarTypeInfo vName dt vDf sClass True)
  modifyState (\p -> p {topLevelScopeIdent = M.insert vName typeInfo (topLevelScopeIdent p)})
  modifyState (\p -> p {currentScopeIdent = M.insert vName typeInfo (currentScopeIdent p)})

getInitialiser :: ParsecT String u IO TypedExpr -> ParsecT String u IO (Maybe TypedExpr)
getInitialiser parser = do
  maybeEqual <- optionMaybe $ try equalLex
  initialiser <- case maybeEqual of
    Just _ -> Just <$> parser
    _ -> pure Nothing
  void semiColParser
  pure $ foldToConstExpr <$> initialiser

topLevelVarDeclarationParser :: ParsecT String ParseInfo IO VarTypeInfo
topLevelVarDeclarationParser = do
  (sc, baseType) <- declarationSpecifierParser False ([], [])
  (vName, declarator) <- declaratorParser (show baseType) "" [id]
  let varType = foldl1 (.) declarator baseType
  topIdentMap <- topLevelScopeIdent <$> getState
  when (M.member vName topIdentMap && isFuncIdentifier (topIdentMap M.! vName)) $
    unexpected $ vName ++ ". Already defined"
  when (M.member vName topIdentMap && isVarIdentifier (topIdentMap M.! vName)
    && (variableType (vti (topIdentMap M.! vName)) /= varType
      || not (topLevelVarStorageClassCheck (varStoreClass (vti (topIdentMap M.! vName))) sc))) $
    unexpected $ vName ++ ". Type mismatch to previous declaration"
  initialiser <- fmap (foldToConstExpr . (`cvtTypedExpr` varType)) <$> getInitialiser exprParser
  unless (isNothing initialiser || isConstantTypedExpr (fromJust initialiser)) $
    unexpected "non constant initialiser"
  when (isJust initialiser) $
    checkPointerInit (fromJust initialiser) varType (Just Static)
  if M.member vName topIdentMap
    then do
      VarIdentifier (VarTypeInfo _ vType vDefine sClass _) <- pure $ topIdentMap M.! vName
      when (isJust vDefine && isJust initialiser) $
        unexpected "variable redefine"
      let newSClass = if sc == Just Extern then sClass else sc
      let define = if isJust vDefine then vDefine else initialiser
      updateTopLevelVarCurrentVar vType vName initialiser sc
      pure $ VarTypeInfo vName vType define newSClass True
    else do
      updateTopLevelVarCurrentVar varType vName initialiser sc
      pure $ VarTypeInfo vName varType initialiser sc True

localExternVarHandle :: DT -> Maybe StorageClass -> IdentifierName -> ParsecT String ParseInfo IO VarTypeInfo
localExternVarHandle varType sc vName  = do
  void semiColParser
  parseInfo <- getState
  top <- topLevelScopeIdent <$> getState
  when (M.member vName top && variableType (vti (top M.! vName)) /= varType) $
    unexpected "different data type"
  unless (M.member vName $ topLevelScopeIdent parseInfo)
      $ updateTopLevelVarCurrentVar varType vName Nothing sc
  modifyState $ \p -> p {currentScopeIdent = M.insert vName
    (VarIdentifier (VarTypeInfo vName varType Nothing sc $ topLevel p)) $ currentScopeIdent p}
  pure $ VarTypeInfo vName varType Nothing sc $ topLevel parseInfo

localPlainVarHandle :: DT -> Maybe StorageClass -> IdentifierName -> ParsecT String ParseInfo IO VarTypeInfo
localPlainVarHandle varType sc vName = do
  parseInfo <- getState
  let varId = currentVarId parseInfo
      newVarName = vName ++ "#" ++ show varId
      newVarMap = M.insert vName
        (VarIdentifier (VarTypeInfo newVarName varType Nothing sc $ topLevel parseInfo)) $
        currentScopeIdent parseInfo
  putState $ parseInfo {currentVarId = varId + 1, currentScopeIdent = newVarMap}
  initialiser <- getInitialiser exprParser
  let cvtinitialiser = foldToConstExpr . (`cvtTypedExpr` varType) <$> initialiser
  when (isJust initialiser) $
    checkImplicitCast (fromJust initialiser) varType
  when (isJust initialiser) $
    checkPointerInit (fromJust initialiser) varType sc
  pure $ VarTypeInfo newVarName varType cvtinitialiser sc $ topLevel parseInfo

localStaticVarHandle :: DT -> Maybe StorageClass -> IdentifierName -> ParsecT String ParseInfo IO VarTypeInfo
localStaticVarHandle varType sc vName = do
  initialiser <- fmap (foldToConstExpr . (`cvtTypedExpr` varType)) <$> getInitialiser exprParser
  unless (isNothing initialiser || isConstantTypedExpr (fromJust initialiser)) $
    unexpected "non constant initialiser"
  parseInfo <- getState
  let varId = globalVarId parseInfo
      newVarName = vName ++ "." ++ show varId
      newLocalVarMap = M.insert vName
        (VarIdentifier (VarTypeInfo newVarName varType Nothing sc (topLevel parseInfo))) $
        currentScopeIdent parseInfo
      newGlobalVarMap = M.insert newVarName
        (VarIdentifier (VarTypeInfo newVarName varType initialiser sc (topLevel parseInfo))) $
        topLevelScopeIdent parseInfo
  putState $ parseInfo {globalVarId = varId + 1,
    currentScopeIdent = newLocalVarMap, topLevelScopeIdent = newGlobalVarMap}
  pure $ VarTypeInfo newVarName varType initialiser sc $ topLevel parseInfo

funcDeclaratorArgListParser :: ParsecT String ParseInfo IO (DT -> DT)
funcDeclaratorArgListParser = DTFuncType <$> argListParser

declaratorParser :: String -> IdentifierName -> [DT -> DT] ->
  ParsecT String ParseInfo IO (IdentifierName, [DT -> DT])
declaratorParser preTok identiferName preDTs = do
  nextChar <- lookAhead $ try (spaces >> anyChar)
  case nextChar of
    '(' -> do
      void openPParser
      (newIdentName, innerType) <- declaratorParser "(" identiferName preDTs
      void closePParser
      afterClosePChar <- lookAhead $ try (spaces >> anyChar)
      maybeFuncType <- case afterClosePChar of
        '(' -> do
          when (isFuncDT $ last innerType (DTInternal TInt)) $
            unexpected "function returning function"
          parseInfo <- getState
          funcDeclaratorArgListParser <* putState parseInfo
        _ -> pure id
      pure (newIdentName, innerType ++ [maybeFuncType])
    '*' -> do
      void mulLex
      (newIdentName, newType) <- declaratorParser "*" identiferName preDTs
      upcomingChar <- lookAhead $ try (spaces >> anyChar)
      maybeFuncType <- case upcomingChar of
        '(' -> do
          parseInfo <- getState
          funcDeclaratorArgListParser <* putState parseInfo
        _ -> pure id
      pure (newIdentName, newType ++ [maybeFuncType, DTPointer])
    ')' -> do
      case preTok of
        "(" -> pure (identiferName, [DTFuncType []])
        _ -> pure (identiferName, [id])
    _ -> do
      newIdentName <- lookAhead (try closePParser <|> try identifierParser) <|> pure ""
      case newIdentName of
        "" -> anyChar >>= unexpected . pure
        ")" -> pure (identiferName, [id])
        _ -> do
          void identifierParser
          afterIdentChar <- lookAhead $ try (spaces >> anyChar)
          maybeFuncType <- case afterIdentChar of
            '(' -> do
              parseInfo <- getState
              funcDeclaratorArgListParser <* putState parseInfo
            _ -> pure id
          pure (newIdentName, [maybeFuncType])

varDeclarationParser :: ParsecT String ParseInfo IO VarTypeInfo
varDeclarationParser = do
  (sc, baseType) <- declarationSpecifierParser False ([], [])
  (vName, declarator) <- declaratorParser (show baseType) "" [id]
  let varType = foldl1 (.) declarator baseType
  when (isFuncDT varType) $
    unexpected "function declaration"
  current <- currentScopeIdent <$> getState
  topIdentMap <- topLevelScopeIdent <$> getState
  when (M.member vName current
    && (isFuncIdentifier (current M.! vName)
      || (varStoreClass (vti (current M.! vName)) /= Just Extern) || sc /= Just Extern)) $
    unexpected $ "Variable redeclare: " ++ vName
  when (sc == Just Extern && M.member vName topIdentMap && isFuncIdentifier (topIdentMap M.! vName)) $
    unexpected $ vName ++ ". Already defined"
  if sc == Just Extern
    then localExternVarHandle varType sc vName
    else if isNothing sc
      then localPlainVarHandle varType sc vName
    else localStaticVarHandle varType sc vName

expressionParser :: ParsecT String ParseInfo IO Statement
expressionParser = Expression . foldToConstExpr <$> exprParser

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
  pure $ While (foldToConstExpr condition) whileBody $ (!! 0) jumpL

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
  pure $ DoWhile doWhileBody (foldToConstExpr condition) $ (!! 0) jumpL

forInitParser :: ParsecT String ParseInfo IO ForInit
forInitParser = do
  maybeType <- lookAhead $ optionMaybe $ declarationSpecifierParser False ([], storageClassSpecifierStrList)
  if isJust maybeType
      then InitDecl <$> varDeclarationParser
      else InitExpr . fmap foldToConstExpr <$> optionMaybe exprParser <* semiColParser

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

dtCvtRead :: Num c => DT -> String -> c
dtCvtRead dt = case dt of
  DTInternal TChar -> fromIntegral . (read :: String -> Int8)
  DTInternal TShort -> fromIntegral . (read :: String -> Int16)
  DTInternal TInt -> fromIntegral . (read :: String -> Int32)
  DTInternal TLong -> fromIntegral . (read :: String -> Int64)
  DTInternal TUInt -> fromIntegral . (read :: String -> Word32)
  DTInternal TULong -> fromIntegral . (read :: String -> Word64)
  _ -> undefined

caseParser :: ParsecT String ParseInfo IO Statement
caseParser = do
  jL <- dropWhile isLoopLabel <$> getJumpLabel
  preJLabel <- takeWhile isLoopLabel <$> getJumpLabel
  case jL of
    SwitchLabel jLabel caseMap : lbs -> do
      void keyCaseParser
      sType <- switchValSize <$> getState
      let cvtRead = dtCvtRead sType
      val <- cvtRead <$> integerParser
      if M.member val caseMap
        then unexpected $ show val ++ ": Already defined."
        else do
          void colonParser
          lId <- getNextLabelId <* bumpLabelId
          let cLabel = if val < 0
              then "case.m" ++ show (abs val) ++ "." ++ show lId
              else "case." ++ show val ++ "." ++ show lId
          modifyState (\p -> p {
            jumpLabel = preJLabel ++ SwitchLabel jLabel
              (M.insert val cLabel caseMap) : lbs})
          state <- statementParser
          pure $ Case state cLabel
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

isIntDT :: DT -> Bool
isIntDT dt = isPointerDT dt || dt `elem` [DTInternal TChar, DTInternal TShort, DTInternal TInt, DTInternal TLong,
  DTInternal TUChar, DTInternal TUShort, DTInternal TUInt, DTInternal TULong]

switchParser :: ParsecT String ParseInfo IO Statement
switchParser = do
  parseInfo <- getState
  updateVarMapForNewScope parseInfo M.empty
  void keySwitchParser
  makeSwitchLabel
  expr@(TExpr _ dt) <- parenExprParser
  unless (isIntDT dt) $ unexpected "non integer type"
  modifyState $ \p -> p{switchValSize = dt}
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
  maybeType <- lookAhead $ optionMaybe $ try symbolExtract
  if isJust maybeType && fromJust maybeType `elem` declarationSpecifierStrList
    then D <$> declareParser
    else S <$> statementParser

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

compareFunTypeDeclare :: FuncTypeInfo -> FuncTypeInfo -> Bool
compareFunTypeDeclare (FuncTypeInfo lFName lFt _ _ _) (FuncTypeInfo rFName rFt _ _ _) =
  lFt == rFt && lFName == rFName

checkForFuncTypeConflict :: ParseInfo -> Declaration -> ParsecT String ParseInfo IO ()
checkForFuncTypeConflict parseInfo declare = case declare of
  FunctionDeclaration (FuncTypeInfo fn fType _ sc _) -> do
    let current = currentScopeIdent parseInfo
    let outer = outerScopeIdent parseInfo
    let top = topLevelScopeIdent parseInfo
    let newFuncType = FuncTypeInfo fn fType Nothing sc 0
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

isFuncDT :: DT -> Bool
isFuncDT dt = case dt of 
  DTFuncType {} -> True
  _ -> False

isPointerDT :: DT -> Bool
isPointerDT dt = case dt of
  DTPointer {} -> True
  _ -> False

funcNotYetDefined :: String -> ParseInfo -> Bool
funcNotYetDefined name parseInfo =
  let current = currentScopeIdent parseInfo in
    M.member name current
      && isFuncIdentifier (current M.! name)
      && isNothing (funcDefine (fti (current M.! name)))

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

addFuncDeclarationIfNeed :: FuncTypeInfo -> ParsecT String ParseInfo IO ()
addFuncDeclarationIfNeed newTypeInfo@(FuncTypeInfo name _ _ _ _) = do
  parseInfo <- getState
  unless (M.member name (outerScopeIdent parseInfo) || M.member name (topLevelScopeIdent parseInfo)) $
    modifyState (\p -> p {outerScopeIdent = M.insert name (FuncIdentifier newTypeInfo) (outerScopeIdent p),
      topLevelScopeIdent = M.insert name (FuncIdentifier newTypeInfo) (topLevelScopeIdent p)})

withFunctionBody :: IdentifierType -> Bool
withFunctionBody typeIdentifier = case typeIdentifier of
  FuncIdentifier (FuncTypeInfo _ _ (Just _) _ _) -> True
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
  (sc, baseType) <- declarationSpecifierParser True ([], [])
  parseInfo <- getState
  updateVarMapForFuncBlockScope
  toplvl <- topLevel <$> getState
  (name, declarator) <- declaratorParser (show baseType) "" [id]
  let fType = foldl1 (.) declarator baseType
  void $ checkForFuncNameConflict name
  checkFuncStorageClass name sc
  checkForFuncTypeConflict parseInfo $
      FunctionDeclaration $ FuncTypeInfo name fType  Nothing sc 1
  addFuncDeclarationIfNeed $ FuncTypeInfo name fType Nothing sc 1
  maybeSemiColon <- lookAhead (try semiColParser <|> try openCurParser)
  modifyState (\p -> p {funcReturnType = retType fType})
  block <- case maybeSemiColon of
    ";" -> semiColParser >> pure Nothing
    "{" -> checkFuncDefinition name parseInfo >>
      Just <$> blockParser (M.fromList (map (\(dt, vName) ->
        (vName, VarIdentifier (VarTypeInfo vName dt Nothing sc toplvl))) (argList fType)))
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
  updateFuncInfoIfNeed name $ FuncIdentifier $ FuncTypeInfo name fType block newSc nVarId
  pure $ FunctionDeclaration $ FuncTypeInfo name fType block newSc nVarId

declareParser :: ParsecT String ParseInfo IO Declaration
declareParser = do
  maybeSpecifier <- lookAhead $ optionMaybe $ try $ declarationSpecifierParser False ([], [])
  case maybeSpecifier of
    Just (_, rType) -> do
      maybeDeclarator <- do
        parseInfo <- getState
        updateVarMapForFuncBlockScope
        lookAhead (optionMaybe $ try $ declarationSpecifierParser False ([], []) >>
          declaratorParser (show rType) "" [id]) <* putState parseInfo
      case maybeDeclarator of
        Just (_, d) -> do
          case foldl1 (.) d rType of
            DTFuncType _ _ -> functionDeclareParser
            _ -> do
              toplvl <- topLevel <$> getState
              if toplvl
                then VariableDeclaration <$> topLevelVarDeclarationParser
                else VariableDeclaration <$> varDeclarationParser
        _ ->
          declarationSpecifierParser False ([], []) >> declaratorParser (show rType) "" [id] >>
            unexpected "declarator error"
    _ -> declarationSpecifierParser False ([], []) >> unexpected "specifier error"

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
updateLabelSingle vd@(VariableDeclaration _) _ = vd
updateLabelSingle fd@(FunctionDeclaration (FuncTypeInfo _ _ Nothing _ _)) _ = fd
updateLabelSingle (FunctionDeclaration (FuncTypeInfo n ft (Just (Block bls)) lid sc)) m =
  FunctionDeclaration $ FuncTypeInfo n ft (Just (Block (map (`updateLabelBlockItem` m) bls))) lid sc

updateGotoLabel :: [Declaration] -> [M.Map String String] -> [Declaration]
updateGotoLabel = zipWith updateLabelSingle
