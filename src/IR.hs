-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/06/18 18:09:02 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module IR where

import Parser
import Operation
import Control.Monad.State

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
      irUnaryOp :: UnaryOp,
      irUnarySrc :: IRVal,
      irUnaryDst :: IRVal
    }
  | IRBinary
    {
      irBinaryOp :: BinaryOp,
      irLOperand :: IRVal,
      irROperand :: IRVal,
      irBinaryDst :: IRVal
    }
  | IRCopy
    {
      irCopySrc :: IRVal,
      irCopyDst :: IRVal
    }
  | IRJump {irJumpTarget :: String}
  | IRJumpIfZero {irJumpZVal :: IRVal, irJumpZeroTarget :: String}
  | IRJumpIfNotZero {irJumpNZVal :: IRVal, irJumpNotZeroTarget :: String}
  | IRLabel {labelName :: String}
  deriving (Show, Eq)

data IRVal =
  IRConstant String
  | IRVar String
  deriving (Show, Eq)

cStatmentToIRInstructions :: BlockItem -> State (Int, Int) [IRInstruction]
cStatmentToIRInstructions (S (Return expr)) = exprToReturnIRs expr
cStatmentToIRInstructions (S Null) = pure []
cStatmentToIRInstructions (S (Expression expr)) = exprToExpressionIRs expr
cStatmentToIRInstructions (S (If condition tStat fStat)) = exprToIfIRs condition tStat fStat
cStatmentToIRInstructions (S (Label l stat)) = (IRLabel l :) <$> cStatmentToIRInstructions (S stat)
cStatmentToIRInstructions (S (Goto l)) = pure [IRJump l]
cStatmentToIRInstructions (S (Compound (Block bl))) = concat <$> traverse cStatmentToIRInstructions bl
cStatmentToIRInstructions (D (VariableDecl _ var (Just expr))) =
  cStatmentToIRInstructions (S (Expression (Assignment None (Variable var) expr)))
cStatmentToIRInstructions (D _) = pure []

initIRVarId :: a1 -> (a2, b) -> (a1, b)
initIRVarId s (_, b) = (s, b)

cFuncDefineToIRFuncDefine :: FunctionDefine -> State (Int, Int) IRFunctionDefine
cFuncDefineToIRFuncDefine fd =
  IRFunctionDefine (funName fd) . (++ [IRReturn (IRConstant "0")]) . concat
    <$> (modify (initIRVarId (nextVarId fd)) >>
      mapM cStatmentToIRInstructions (unBlock $ body fd))

cASTToIrAST :: CProgramAST -> State (Int, Int) IRProgramAST
cASTToIrAST = mapM cFuncDefineToIRFuncDefine

bumpOneToVarId :: Num a => (a, b) -> (a, b)
bumpOneToVarId (a, b) = (a + 1, b)

bumpOneToLabelId :: Num b => (a, b) -> (a, b)
bumpOneToLabelId (a, b) = (a, b + 1)

exprToReturnIRs :: Expr -> State (Int, Int) [IRInstruction]
exprToReturnIRs expr = do
  (irs, irVal) <- exprToIRs expr
  pure $ irs ++ [IRReturn irVal]

exprToExpressionIRs :: Expr -> State (Int, Int) [IRInstruction]
exprToExpressionIRs expr = fst <$> exprToIRs expr

exprToIfIRs :: Expr -> Statement -> Maybe Statement -> State (Int, Int) [IRInstruction]
exprToIfIRs condition tStat fStat = do
  (cIRs, cValIR) <- exprToIRs condition
  tStatIRs <- cStatmentToIRInstructions $ S tStat
  (fStatIRs, fLabel) <- do
    labelId <- gets (show . snd) <* modify bumpOneToLabelId
    case fStat of
      Just fs -> do 
        fsIRs <- cStatmentToIRInstructions $ S fs
        dLabelId <- gets (show . snd) <* modify bumpOneToLabelId
        pure ([IRJump $ "ifSkip" ++ dLabelId, IRLabel ("ifFalse" ++ labelId)] ++ fsIRs ++ [IRLabel $ "ifSkip" ++ dLabelId], "ifFalse" ++ labelId)
      _ -> pure ([IRLabel ("ifSkip" ++ labelId)], "ifSkip" ++ labelId)
  pure $ cIRs ++ [IRJumpIfZero cValIR fLabel] ++ tStatIRs ++ fStatIRs

postPrefixToBin :: UnaryOp -> BinaryOp
postPrefixToBin op
  | op `elem` [PostDecrement, PreDecrement] = Minus
  | otherwise = Plus

unaryOperationToIRs :: UnaryOp -> Expr -> State (Int, Int) ([IRInstruction], IRVal)
unaryOperationToIRs op uExpr
  | op `elem` [PostDecrement, PostIncrement] = do
    varId <- gets $ IRVar . show . fst
    modify bumpOneToVarId
    (oldIRs, irVal) <- exprToIRs $ Binary (postPrefixToBin op) uExpr (Constant "1")
    (varIRs, irVar) <- exprToIRs uExpr
    pure (concat [[IRCopy irVar varId], oldIRs, varIRs, [IRCopy irVal irVar]], varId)
  | op `elem` [PreDecrement, PreIncrement] = do
    (oldIRs, irVal) <- exprToIRs $ Binary (postPrefixToBin op) uExpr (Constant "1")
    (varIRs, irVar) <- exprToIRs uExpr
    pure (oldIRs ++ varIRs ++ [IRCopy irVal irVar], irVal)
  | otherwise = do
      (oldIRs, irVal) <- exprToIRs uExpr
      varId <- gets fst
      modify bumpOneToVarId
      pure (oldIRs ++ [IRUnary op irVal $ IRVar $ show varId], IRVar $ show varId)

genJumpIRsIfNeeded :: BinaryOp -> (Int, Int) -> IRVal -> State (Int, Int) [IRInstruction]
genJumpIRsIfNeeded op labelId irVal =
  case op of
    LogicAnd ->
      pure [IRJumpIfZero irVal $ "false_label" ++ show (fst labelId)]
    LogicOr ->
      pure [IRJumpIfNotZero irVal $ "false_label" ++ show (fst labelId)]
    _ -> pure []

genJumpIRsAndLabel :: Int -> (Int, Int) -> IRVal -> IRVal -> State (Int, Int) [IRInstruction]
genJumpIRsAndLabel varId ids fstVal sndVal = do
  pure [IRCopy fstVal $ IRVar $ show varId,
    IRJump $ "end_label" ++ show (snd ids),
    IRLabel $ "false_label" ++ show (fst ids),
    IRCopy sndVal $ IRVar $ show varId,
    IRLabel $ "end_label" ++ show (snd ids)]

genLabelIfNeeded :: BinaryOp -> State (Int, Int) (Int, Int)
genLabelIfNeeded op =
  let getLabels =
        do
          labelId <- gets snd
          modify bumpOneToLabelId
          endLabelId <- gets snd
          modify bumpOneToLabelId
          pure (labelId, endLabelId) in
  case op of
    LogicAnd -> getLabels
    LogicOr -> getLabels
    _ -> pure (-1, -1)

binaryOperationToIRs :: BinaryOp -> Expr -> Expr -> State (Int, Int) ([IRInstruction], IRVal)
binaryOperationToIRs op lExpr rExpr = do
  ids <- genLabelIfNeeded op
  (irsFromLExpr, irValFromLExpr) <- exprToIRs lExpr
  lExprCondJumpIRs <- genJumpIRsIfNeeded op ids irValFromLExpr
  (irsFromRExpr, irValFromRExpr) <- exprToIRs rExpr
  rExprCondJumpIRs <- genJumpIRsIfNeeded op ids irValFromRExpr
  varId <- gets fst
  modify bumpOneToVarId
  resultIRVal <-
    let trueVal = IRConstant "1"
        falseVal = IRConstant "0" in
      case op of
        LogicAnd -> genJumpIRsAndLabel varId ids trueVal falseVal
        LogicOr -> genJumpIRsAndLabel varId ids falseVal trueVal
        _ -> pure [IRBinary op irValFromLExpr irValFromRExpr $ IRVar $ show varId]
  pure (concat
    [irsFromLExpr, lExprCondJumpIRs, irsFromRExpr, rExprCondJumpIRs, resultIRVal],
    IRVar $ show varId)

dropVarName :: String -> String
dropVarName v = drop 1 $ dropWhile (/= '#') v

assignmentToIRs :: BinaryOp -> Expr -> Expr -> State (Int, Int) ([IRInstruction], IRVal)
assignmentToIRs op var rExpr = do
  (rIRs, rVal) <- case op of
    None -> exprToIRs rExpr
    _ -> binaryOperationToIRs op var rExpr
  (varIRs, irVar) <- exprToIRs var
  pure (rIRs ++ varIRs ++ [IRCopy rVal irVar], irVar)

conditionToIRs :: Expr -> Expr -> Expr -> State (Int, Int) ([IRInstruction], IRVal)
conditionToIRs condition tExpr fExpr = do
  (cIRs, cValIR) <- exprToIRs condition
  (tIRs, tValIR) <- exprToIRs tExpr
  (fIRs, fValIR) <- exprToIRs fExpr
  resValIR <- gets (IRVar . show . fst) <* modify bumpOneToVarId
  fLabel <- gets (show . snd) <* modify bumpOneToLabelId
  dLabel <- gets (show . snd) <* modify bumpOneToLabelId
  pure (cIRs ++ [IRJumpIfZero cValIR ("condF" ++ fLabel)]
    ++ tIRs ++ [IRCopy tValIR resValIR, IRJump $ "condD" ++ dLabel , IRLabel $ "condF" ++ fLabel]
    ++ fIRs ++ [IRCopy fValIR resValIR, IRLabel $ "condD" ++ dLabel], resValIR)

exprToIRs :: Expr -> State (Int, Int) ([IRInstruction], IRVal)
exprToIRs expr =
  case expr of
    Constant s -> pure ([], IRConstant s)
    Unary op uExpr -> unaryOperationToIRs op uExpr
    Binary op lExpr rExpr -> binaryOperationToIRs op lExpr rExpr
    Variable var -> pure ([], IRVar (dropVarName var))
    Assignment op (Variable var) rExpr -> assignmentToIRs op (Variable var) rExpr
    Conditional condition tCond fCond -> conditionToIRs condition tCond fCond
    _ -> undefined
