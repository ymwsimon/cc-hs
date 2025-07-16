-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/07/16 10:47:14 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module IR where

import Parser
import Operation
import Control.Monad.State
import qualified Data.Map.Strict as M
import Control.Monad (mapAndUnzipM)
import Data.Char
import Data.Maybe (fromMaybe)

type IRProgramAST = [IRTopLevel]

data IRTopLevel =
  IRFunc {irFuncD :: IRFunctionDefine}
  | IRStaticVar {irStaticVarD :: IRStaticVarDefine}
  deriving (Show, Eq)

data IRFunctionDefine =
  IRFunctionDefine
  {
    irFuncName :: String,
    irFuncGlobal :: Bool,
    irParameter :: [InputArgPair],
    irInstruction :: [IRInstruction]
  }
  deriving (Show, Eq)

data IRStaticVarDefine =
  IRStaticVarDefine {irVarName :: String, irVarGlobal :: Bool,
    irType :: DT, irVarInitVal :: StaticInit}
  deriving (Show, Eq)

data IRInstruction =
  IRReturn IRVal
  | IRSignExtend {signeExtendSrc :: IRVal, signExtendDst :: IRVal}
  | IRTruncate {truncateSrc :: IRVal, truncateDst :: IRVal}
  | IRZeroExtend {zeroExtendSrc :: IRVal, zeroExtenDst :: IRVal}
  | IRDoubleToInt {dToISrc :: IRVal, dToIDst :: IRVal}
  | IRDoubleToUInt {jmpLabels :: [String], dToUISrc :: IRVal, dToUIDst :: IRVal}
  | IRIntToDouble {iToDSrc :: IRVal, iToDDst :: IRVal}
  | IRUIntToDouble {jmpLabels :: [String], uiToDSrc :: IRVal, uiToDDst :: IRVal}
  | IRUnary {irUnaryOp :: UnaryOp, irUnarySrc :: IRVal, irUnaryDst :: IRVal}
  | IRBinary {irBinaryOp :: BinaryOp, irLOperand :: IRVal,
      irROperand :: IRVal, irBinaryDst :: IRVal}
  | IRCopy {irCopySrc :: IRVal, irCopyDst :: IRVal}
  | IRJump {irJumpTarget :: String}
  | IRJumpIfZero {irJumpZVal :: IRVal, irJumpZeroTarget :: String}
  | IRJumpIfNotZero {irJumpNZVal :: IRVal, irJumpNotZeroTarget :: String}
  | IRLabel {labelName :: String}
  | IRFuncCall {irFName :: String, arg :: [IRVal], irFCallDst :: IRVal}
  deriving (Show, Eq)

data IRVal =
  IRConstant DT NumConst
  | IRVar {irVarDT :: DT, irVName :: String}
  deriving (Show, Eq)

data StaticInit =
  ShortInit NumConst
  | UShortInit NumConst
  | IntInit NumConst
  | UIntInit NumConst
  | LongInit NumConst
  | ULongInit NumConst
  | DoubleInit NumConst
  deriving (Show, Eq)

isIRFuncDefine :: IRTopLevel -> Bool
isIRFuncDefine (IRFunc _) = True
isIRFuncDefine _ = False

isIRStaticVarDefine :: IRTopLevel -> Bool
isIRStaticVarDefine (IRStaticVar _) = True
isIRStaticVarDefine _ = False

irValToDT :: IRVal -> DT
irValToDT irVal = case irVal of
  IRConstant dt _ -> dt
  IRVar dt _ -> dt

cStatmentToIRInstructions :: BlockItem -> State (Int, Int) [IRInstruction]
cStatmentToIRInstructions bi = case bi of
  S (Return expr) -> exprToReturnIRs expr
  S Null -> pure []
  S (Expression expr) -> exprToExpressionIRs expr
  S (If condition tStat fStat) -> exprToIfIRs condition tStat fStat
  S (Label _ l stat) ->
    (IRLabel l :) <$> cStatmentToIRInstructions (S stat)
  S (Goto l) -> pure [IRJump l]
  S (Compound (Block bl)) ->
    concat <$> traverse cStatmentToIRInstructions bl
  S (Break l) -> pure [IRJump l]
  S (Continue l) -> pure [IRJump l]
  S (DoWhile bl condition l) -> case l of
    LoopLabel jLabel -> doWhileToIRs bl condition jLabel
    _ -> undefined
  S (While condition bl l) -> case l of
    LoopLabel jLabel -> whileToIRs condition bl jLabel
    _ -> undefined
  S (For forInit condition post bl l) -> case l of
    LoopLabel jLabel -> forToIRs forInit condition post bl jLabel
    _ -> undefined
  S (Switch condition bl l) -> case l of
    SwitchLabel jLabel caseMap -> switchToIRs condition bl jLabel caseMap
    _ -> undefined
  S (Case statement l) -> caseToIRs statement l
  S (Default statement l) -> defaultToIRs statement l
  D (VD ((VariableDeclaration dt var False (Just expr) Nothing))) ->
    cStatmentToIRInstructions (S (Expression
      (TExpr (Assignment (TExpr (Variable var False Nothing) dt) expr) dt)))
  D _ -> pure []

initIRVarId :: a1 -> (a2, b) -> (a1, b)
initIRVarId s (_, b) = (s, b)

hasFuncBody :: Declaration -> Bool
hasFuncBody (FunctionDeclaration _ _ _ (Just _) _ _) = True
hasFuncBody _ = False

cFuncDefineToIRFuncDefine :: Declaration -> State (Int, Int) IRTopLevel
cFuncDefineToIRFuncDefine fd@(FunctionDeclaration _ _ _ (Just bl) _ sc) =
  IRFunc . IRFunctionDefine (funName fd) (sc /= Just Static) (inputArgs fd)
    . (++ [IRReturn (IRConstant (DTInternal TInt) $ ConstInt 0)]) . concat
    <$> (modify (initIRVarId (nextVarId fd)) >>
      mapM cStatmentToIRInstructions (unBlock bl))
cFuncDefineToIRFuncDefine  _ = undefined

cASTToIrAST :: CProgramAST -> IRProgramAST
cASTToIrAST = flip evalState (1, 1) . mapM cFuncDefineToIRFuncDefine . filter hasFuncBody

constantExprToInt :: TypedExpr -> Integer
constantExprToInt e = case e of
  TExpr (Constant (ConstInt i)) _ -> fromIntegral i
  TExpr (Constant (ConstLong l)) _ -> fromIntegral l
  TExpr (Constant (ConstUInt ui)) _ -> fromIntegral ui
  TExpr (Constant (ConstULong ul)) _ -> fromIntegral ul
  TExpr (Constant (ConstDouble d)) _ -> truncate d
  _ -> undefined

constantExprToDouble :: TypedExpr -> Double
constantExprToDouble e = case e of
  TExpr (Constant (ConstDouble d)) _ -> d
  TExpr (Constant (ConstInt i)) _ -> fromIntegral i
  TExpr (Constant (ConstLong l)) _ -> fromIntegral l
  TExpr (Constant (ConstUInt ui)) _ -> fromIntegral ui
  TExpr (Constant (ConstULong ul)) _ -> fromIntegral ul
  _ -> undefined

exprToStaticInit :: TypedExpr -> StaticInit
exprToStaticInit (TExpr e _) = case e of
  Constant (ConstInt i) -> IntInit $ ConstInt i
  Constant (ConstLong l) -> LongInit $ ConstLong l
  Constant (ConstUInt ui) -> UIntInit $ ConstUInt ui
  Constant (ConstULong ul) -> ULongInit $ ConstULong ul
  Constant (ConstDouble d) -> DoubleInit $ ConstDouble d
  _ -> undefined

staticInitToInt :: StaticInit -> Integer
staticInitToInt (IntInit (ConstInt i)) = i
staticInitToInt (UIntInit (ConstUInt ui)) = ui
staticInitToInt (LongInit (ConstLong l)) = l
staticInitToInt (ULongInit (ConstULong ul)) = ul
staticInitToInt (DoubleInit (ConstDouble d)) = truncate d
staticInitToInt _ = undefined

staticInitToDouble :: StaticInit -> Double
staticInitToDouble (IntInit (ConstInt i)) = fromIntegral i
staticInitToDouble (UIntInit (ConstUInt ui)) = fromIntegral ui
staticInitToDouble (LongInit (ConstLong l)) = fromIntegral l
staticInitToDouble (ULongInit (ConstULong ul)) = fromIntegral ul
staticInitToDouble (DoubleInit (ConstDouble d)) = d
staticInitToDouble _ = undefined

staticVarConvertion :: M.Map String IdentifierType -> [IRTopLevel]
staticVarConvertion m = map IRStaticVar .
  concatMap (identToStaticVar . snd) $ M.toList $ M.filter isVarIdentifier m
  where identToStaticVar ident = case ident of
          VarIdentifier dt vName _ expr (Just Static) -> if dt == DTInternal TDouble
            then [IRStaticVarDefine vName False dt
              (exprToStaticInit $ fromMaybe (TExpr (Constant $ ConstDouble 0) (DTInternal TDouble)) expr)]
            else [IRStaticVarDefine vName False dt
              (exprToStaticInit $ fromMaybe (TExpr (Constant $ ConstInt 0) (DTInternal TInt)) expr)]
          VarIdentifier dt vName topLvl expr Nothing -> if dt == DTInternal TDouble
            then [IRStaticVarDefine vName topLvl dt
              (exprToStaticInit $ fromMaybe (TExpr (Constant $ ConstDouble 0) (DTInternal TDouble)) expr)]
            else [IRStaticVarDefine vName topLvl dt
              (exprToStaticInit $ fromMaybe (TExpr (Constant $ ConstInt 0) (DTInternal TInt)) expr)]
          _ -> []

bumpOneToVarId :: Num a => (a, b) -> (a, b)
bumpOneToVarId (a, b) = (a + 1, b)

bumpOneToLabelId :: Num b => (a, b) -> (a, b)
bumpOneToLabelId (a, b) = (a, b + 1)

exprToReturnIRs :: TypedExpr -> State (Int, Int) [IRInstruction]
exprToReturnIRs expr = do
  (irs, irVal) <- exprToIRs expr
  pure $ irs ++ [IRReturn irVal]

exprToExpressionIRs :: TypedExpr -> State (Int, Int) [IRInstruction]
exprToExpressionIRs expr = fst <$> exprToIRs expr

exprToIfIRs :: TypedExpr -> Statement -> Maybe Statement -> State (Int, Int) [IRInstruction]
exprToIfIRs condition tStat fStat = do
  (cIRs, cValIR) <- exprToIRs condition
  tStatIRs <- cStatmentToIRInstructions $ S tStat
  (fStatIRs, fLabel) <- do
    lId <- gets (show . snd) <* modify bumpOneToLabelId
    case fStat of
      Just fs -> do 
        fsIRs <- cStatmentToIRInstructions $ S fs
        dLabelId <- gets (show . snd) <* modify bumpOneToLabelId
        pure ([IRJump $ "ifSkip" ++ dLabelId, IRLabel ("ifFalse" ++ lId)] ++
          fsIRs ++ [IRLabel $ "ifSkip" ++ dLabelId], "ifFalse" ++ lId)
      _ -> pure ([IRLabel ("ifSkip" ++ lId)], "ifSkip" ++ lId)
  pure $ cIRs ++ [IRJumpIfZero cValIR fLabel] ++ tStatIRs ++ fStatIRs

postPrefixToBin :: UnaryOp -> BinaryOp
postPrefixToBin op
  | op `elem` [PostDecrement, PreDecrement] = Minus
  | otherwise = Plus

unaryOperationToIRs :: UnaryOp -> DT -> TypedExpr -> State (Int, Int) ([IRInstruction], IRVal)
unaryOperationToIRs op dt uExpr
  | op `elem` [PostDecrement, PostIncrement] = do
    varId <- gets $ IRVar dt . show . fst
    modify bumpOneToVarId
    (oldIRs, irVal) <-
      let c = if isFloatDT dt then makeConstantTEFloatWithDT 1.0 dt else makeConstantTEIntWithDT 1 dt in
      exprToIRs $ TExpr
        (Binary (postPrefixToBin op) uExpr c) $ tDT uExpr
    (varIRs, irVar) <- exprToIRs uExpr
    pure (concat [[IRCopy irVar varId], oldIRs, varIRs, [IRCopy irVal irVar]], varId)
  | op `elem` [PreDecrement, PreIncrement] = do
    (oldIRs, irVal) <- let c = if isFloatDT dt then makeConstantTEFloatWithDT 1.0 dt else makeConstantTEIntWithDT 1 dt in
      exprToIRs $ TExpr 
        (Binary (postPrefixToBin op) uExpr c) $ tDT uExpr
    (varIRs, irVar) <- exprToIRs uExpr
    pure (oldIRs ++ varIRs ++ [IRCopy irVal irVar], irVal)
  | otherwise = do
      (oldIRs, irVal) <- exprToIRs uExpr
      varId <- gets fst
      modify bumpOneToVarId
      pure (oldIRs ++ [IRUnary op irVal $ IRVar dt $ show varId], IRVar dt $ show varId)

genJumpIRsIfNeeded :: BinaryOp -> (Int, Int) -> IRVal -> State (Int, Int) [IRInstruction]
genJumpIRsIfNeeded op lId irVal = case op of
    LogicAnd ->
      pure [IRJumpIfZero irVal $ "false_label" ++ show (fst lId)]
    LogicOr ->
      pure [IRJumpIfNotZero irVal $ "false_label" ++ show (fst lId)]
    _ -> pure []

genJumpIRsAndLabel :: Int -> (Int, Int) -> IRVal -> IRVal -> State (Int, Int) [IRInstruction]
genJumpIRsAndLabel varId ids fstVal sndVal = do
  pure [IRCopy fstVal $ IRVar (DTInternal TInt) $ show varId,
    IRJump $ "end_label" ++ show (snd ids),
    IRLabel $ "false_label" ++ show (fst ids),
    IRCopy sndVal $ IRVar (DTInternal TInt) $ show varId,
    IRLabel $ "end_label" ++ show (snd ids)]

genLabelIfNeeded :: BinaryOp -> State (Int, Int) (Int, Int)
genLabelIfNeeded op =
  let getLabels = do
          lId <- gets snd
          modify bumpOneToLabelId
          endLabelId <- gets snd
          modify bumpOneToLabelId
          pure (lId, endLabelId) in
  case op of
    LogicAnd -> getLabels
    LogicOr -> getLabels
    _ -> pure (-1, -1)

binaryOperationToIRs :: BinaryOp -> DT -> TypedExpr -> TypedExpr -> State (Int, Int) ([IRInstruction], IRVal)
binaryOperationToIRs op dt lExpr rExpr = do
  ids <- genLabelIfNeeded op
  (irsFromLExpr, irValFromLExpr) <- exprToIRs lExpr
  lExprCondJumpIRs <- genJumpIRsIfNeeded op ids irValFromLExpr
  (irsFromRExpr, irValFromRExpr) <- exprToIRs rExpr
  rExprCondJumpIRs <- genJumpIRsIfNeeded op ids irValFromRExpr
  varId <- gets fst
  modify bumpOneToVarId
  resultIRVal <-
    let trueVal = IRConstant (DTInternal TInt) $ ConstInt 1
        falseVal = IRConstant (DTInternal TInt) $ ConstInt 0 in
      case op of
        LogicAnd -> genJumpIRsAndLabel varId ids trueVal falseVal
        LogicOr -> genJumpIRsAndLabel varId ids falseVal trueVal
        _ -> pure [IRBinary op irValFromLExpr irValFromRExpr $ IRVar dt $ show varId]
  pure (concat
    [irsFromLExpr, lExprCondJumpIRs, irsFromRExpr, rExprCondJumpIRs, resultIRVal],
    IRVar dt $ show varId)

dropVarName :: String -> String
dropVarName v = if '#' `elem` v then drop 1 $ dropWhile (/= '#') v else v

assignmentToIRs :: TypedExpr -> TypedExpr -> State (Int, Int) ([IRInstruction], IRVal)
assignmentToIRs var rExpr = do
  (rIRs, rVal) <- exprToIRs rExpr
  (varIRs, irVar) <- exprToIRs var
  pure (rIRs ++ varIRs ++ [IRCopy rVal irVar], irVar)

conditionToIRs :: DT -> TypedExpr -> TypedExpr -> TypedExpr -> State (Int, Int) ([IRInstruction], IRVal)
conditionToIRs dt condition tExpr fExpr = do
  (cIRs, cValIR) <- exprToIRs condition
  (tIRs, tValIR) <- exprToIRs tExpr
  (fIRs, fValIR) <- exprToIRs fExpr
  resValIR <- gets (IRVar dt . show . fst) <* modify bumpOneToVarId
  fLabel <- gets (show . snd) <* modify bumpOneToLabelId
  dLabel <- gets (show . snd) <* modify bumpOneToLabelId
  pure (cIRs ++ [IRJumpIfZero cValIR ("condF" ++ fLabel)]
    ++ tIRs ++ [IRCopy tValIR resValIR, IRJump $ "condD" ++ dLabel , IRLabel $ "condF" ++ fLabel]
    ++ fIRs ++ [IRCopy fValIR resValIR, IRLabel $ "condD" ++ dLabel], resValIR)

doWhileToIRs :: Statement -> TypedExpr -> (String, String, String) -> State (Int, Int) [IRInstruction]
doWhileToIRs bl condition (sLabel, cLabel, dLabel) = do
  blIRs <- cStatmentToIRInstructions $ S bl
  (exprIRs, exprIRVal) <- exprToIRs condition
  pure $ IRLabel sLabel : blIRs ++ [IRLabel cLabel] ++
    exprIRs ++ [IRJumpIfNotZero exprIRVal sLabel, IRLabel dLabel]

whileToIRs :: TypedExpr -> Statement -> (String, String, String) -> State (Int, Int) [IRInstruction]
whileToIRs condition bl (_, cLabel, dLabel) = do
  (exprIRs, exprIRVal) <- exprToIRs condition
  blIRs <- cStatmentToIRInstructions $ S bl
  pure $ IRLabel cLabel : exprIRs ++
    [IRJumpIfZero exprIRVal dLabel] ++
    blIRs ++ [IRJump cLabel, IRLabel dLabel]

forInitToIRs :: ForInit -> State (Int, Int) [IRInstruction]
forInitToIRs fi = case fi of
  InitDecl d -> cStatmentToIRInstructions $ D $ VD d
  InitExpr (Just expr) -> cStatmentToIRInstructions $ S $ Expression expr
  _ -> pure []

forToIRs :: ForInit -> Maybe TypedExpr -> Maybe TypedExpr -> Statement ->
  (String, String, String) -> State (Int, Int) [IRInstruction]
forToIRs forInit condition post bl (sLabel, cLabel, dLabel) = do
  forInitIRs <- forInitToIRs forInit
  conditionIRs <- case condition of
    Just c -> do
      (exprIRs, exprIRVal) <- exprToIRs c
      pure $ IRLabel sLabel : exprIRs ++ [IRJumpIfZero exprIRVal dLabel]
    _ -> pure [IRLabel sLabel]
  postIRs <- case post of
    Just p -> do
      pIRs <- cStatmentToIRInstructions $ S $ Expression p
      pure $ IRLabel cLabel : pIRs ++ [IRJump sLabel]
    _ -> pure [IRLabel cLabel, IRJump sLabel]
  blIRs <- cStatmentToIRInstructions $ S bl
  pure $ forInitIRs ++ conditionIRs ++ blIRs ++ postIRs ++ [IRLabel dLabel]

caseMapToIRJump :: IRVal -> IRVal -> M.Map Integer String -> [IRInstruction]
caseMapToIRJump irVal resIRVal m = concatMap caseToIRJump $ M.toList m
  where caseToIRJump (val, l) =
          let irConst = if irValToDT irVal ==DTInternal TLong
                          then IRConstant (DTInternal TLong) $ ConstLong val
                          else IRConstant (DTInternal TInt) $ ConstInt $ fromIntegral val in
          [IRBinary EqualRelation irVal irConst resIRVal,
            IRJumpIfNotZero resIRVal l]

switchToIRs :: TypedExpr -> Statement -> (Maybe String, String) ->
  M.Map Integer String -> State (Int, Int) [IRInstruction]
switchToIRs c@(TExpr _ cDt) bl (defaultLabel, doneLabel) caseMap = do
  (exprIRs, exprIRVal) <- exprToIRs c
  varId <- gets fst <* modify bumpOneToVarId
  blIRs <- cStatmentToIRInstructions $ S bl
  defaultIRs <- case defaultLabel of
    Just l -> pure [IRJump l]
    _ -> pure []
  let caseIRs = caseMapToIRJump exprIRVal (IRVar cDt (show varId)) caseMap
  pure $ exprIRs ++ caseIRs ++ defaultIRs ++ [IRJump doneLabel] ++ blIRs ++ [IRLabel doneLabel]

caseToIRs :: Statement -> String -> State (Int, Int) [IRInstruction]
caseToIRs statement l = do
  stateIRs <- cStatmentToIRInstructions $ S statement
  pure $ IRLabel l : stateIRs

defaultToIRs :: Statement -> String -> State (Int, Int) [IRInstruction]
defaultToIRs statement l = do
  stateIRs <- cStatmentToIRInstructions $ S statement
  pure $ IRLabel l : stateIRs

funcCallToIRs :: String -> DT -> [TypedExpr] -> State (Int, Int) ([IRInstruction], IRVal)
funcCallToIRs name dt exprs = do
  varId <- gets $ IRVar dt . show . fst
  modify bumpOneToVarId
  (irs, irVal) <- mapAndUnzipM exprToIRs exprs
  pure (concat irs ++ [IRFuncCall name irVal varId], varId)

truncateIRVal :: IRVal -> DT -> IRVal
truncateIRVal irVal dt = case irVal of
  og@(IRConstant _ (ConstLong l)) -> case dt of
    DTInternal TInt -> IRConstant dt (ConstInt (fromIntegral l))
    DTInternal TLong -> og
    _ -> undefined
  _ -> irVal

castToIRs :: DT -> TypedExpr -> State (Int, Int) ([IRInstruction], IRVal)
castToIRs dt te@(TExpr _ tdt)
  | dt == tdt = exprToIRs te
  | otherwise = do
    (teIRs, teIRVal) <- exprToIRs te
    varId <- gets (IRVar dt . show . fst) <* modify bumpOneToVarId
    lbsId <- if isFloatDT dt && isUnsigned tdt || isUnsigned dt && isFloatDT tdt
      then do
        l1 <- gets (show . snd) <* modify bumpOneToLabelId
        l2 <- gets (show . snd) <* modify bumpOneToLabelId
        pure [l1, l2]
      else pure []
    let op
          | isFloatDT dt && isSignedInteger tdt = IRIntToDouble
          | isFloatDT dt && isUnsigned tdt = IRUIntToDouble lbsId
          | isSignedInteger dt && isFloatDT tdt = IRDoubleToInt
          | isUnsigned dt && isFloatDT tdt = IRDoubleToUInt lbsId
          | getDTSize dt == getDTSize tdt = IRCopy
          | getDTSize dt < getDTSize tdt = IRTruncate
          | isSignedInteger tdt = IRSignExtend
          | otherwise = IRZeroExtend
    pure (teIRs ++ [op teIRVal varId], varId)

exprToIRs :: TypedExpr -> State (Int, Int) ([IRInstruction], IRVal)
exprToIRs (TExpr expr dt) = case expr of
  Constant (ConstShort s) -> pure ([], IRConstant (DTInternal TShort) $ ConstShort s)
  Constant (ConstUShort s) -> pure ([], IRConstant (DTInternal TUShort) $ ConstUShort s)
  Constant (ConstInt s) -> pure ([], IRConstant (DTInternal TInt) $ ConstInt s)
  Constant (ConstUInt s) -> pure ([], IRConstant (DTInternal TUInt) $ ConstUInt s)
  Constant (ConstLong s) -> pure ([], IRConstant (DTInternal TLong) $ ConstLong s)
  Constant (ConstULong s) -> pure ([], IRConstant (DTInternal TULong) $ ConstULong s)
  Constant (ConstDouble s) -> pure ([], IRConstant (DTInternal TDouble) $ ConstDouble s)
  Unary op uExpr -> unaryOperationToIRs op dt uExpr
  Binary op lExpr rExpr -> binaryOperationToIRs op dt lExpr rExpr
  Variable var _ _ -> pure ([], IRVar dt var)
  Assignment vExpr rExpr ->
    assignmentToIRs vExpr rExpr
  Conditional condition tCond fCond -> conditionToIRs dt condition tCond fCond
  FunctionCall name exprs -> funcCallToIRs name dt exprs
  Cast cDt cExpr -> castToIRs cDt cExpr

extractVarId :: IRInstruction -> [Int]
extractVarId instr = case instr of
  IRReturn v -> [getVarId v]
  IRUnary _ s d -> [getVarId s, getVarId d]
  IRBinary _ l r d -> [getVarId l, getVarId r, getVarId d]
  IRCopy s d -> [getVarId s, getVarId d]
  IRJump _ -> []
  IRJumpIfZero v _ -> [getVarId v]
  IRJumpIfNotZero v _ -> [getVarId v]
  IRLabel _ -> []
  IRFuncCall _ args d -> map getVarId (d : args)
  IRSignExtend s d -> [getVarId s, getVarId d]
  IRTruncate s d -> [getVarId s, getVarId d]
  IRZeroExtend s d -> [getVarId s, getVarId d]
  IRDoubleToInt s d -> [getVarId s, getVarId d]
  IRDoubleToUInt _ s d -> [getVarId s, getVarId d]
  IRIntToDouble s d -> [getVarId s, getVarId d]
  IRUIntToDouble _ s d -> [getVarId s, getVarId d]
  where getVarId i = case i of
          IRConstant _ _ -> 0
          IRVar _ iv -> if all isDigit iv then read iv else 0

getMaxStackVarId :: [IRInstruction] -> Int
getMaxStackVarId = maximum . concatMap extractVarId 
