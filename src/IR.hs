-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/04/10 23:39:38 by mayeung          ###   ########.fr       --
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

cStatmentToIRInstructions :: Statment -> State (Int, Int) [IRInstruction]
cStatmentToIRInstructions (Return expr) = exprToReturnIRs expr
cStatmentToIRInstructions _ = undefined

resetIRValId :: Num c => (a, b) -> (c, b)
resetIRValId (_, b) = (1, b) 

cFuncDefineToIRFuncDefine :: FunctionDefine -> State (Int, Int) IRFunctionDefine
cFuncDefineToIRFuncDefine fd =
  IRFunctionDefine (funName fd) . concat
    <$> (modify resetIRValId >> mapM cStatmentToIRInstructions (body fd))

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

unaryOperationToIRs :: UnaryOp -> Expr -> State (Int, Int) ([IRInstruction], IRVal)
unaryOperationToIRs op uExpr =  do
  (oldIRs, irVal) <- exprToIRs uExpr
  varId <- gets fst
  modify bumpOneToVarId
  pure (oldIRs ++ [IRUnary op irVal $ IRVar $ show varId], IRVar $ show varId)

genJumpIRsIfNeeded :: BinaryOp -> IRVal -> State (Int, Int) [IRInstruction]
genJumpIRsIfNeeded op irVal =
  case op of
    LogicAnd -> do
      labelId <- gets snd
      pure [IRJumpIfZero irVal $ "false_label" ++ show labelId]
    LogicOr -> do
      labelId <- gets snd
      pure [IRJumpIfNotZero irVal $ "false_label" ++ show labelId]
    _ -> pure []

genJumpIRsAndLabel :: Int -> IRVal -> IRVal -> State (Int, Int) [IRInstruction]
genJumpIRsAndLabel varId fstVal sndVal = do
  labelId <- gets snd
  modify bumpOneToLabelId
  endLabelId <- gets snd
  modify bumpOneToLabelId
  pure [IRCopy fstVal $ IRVar $ show varId,
    IRJump $ "end_label" ++ show endLabelId,
    IRLabel $ "false_label" ++ show labelId,
    IRCopy sndVal $ IRVar $ show varId,
    IRLabel $ "end_label" ++ show endLabelId]

binaryOperationToIRs :: BinaryOp -> Expr -> Expr -> State (Int, Int) ([IRInstruction], IRVal)
binaryOperationToIRs op lExpr rExpr = do
  (irsFromLExpr, irValFromLExpr) <- exprToIRs lExpr
  lExprCondJumpIRs <- genJumpIRsIfNeeded op irValFromLExpr
  (irsFromRExpr, irValFromRExpr) <- exprToIRs rExpr
  rExprCondJumpIRs <- genJumpIRsIfNeeded op irValFromRExpr
  varId <- gets fst
  modify bumpOneToVarId
  resultIRVal <-
    let trueVal = IRConstant "1"
        falseVal = IRConstant "0" in
      case op of
        LogicAnd -> genJumpIRsAndLabel varId trueVal falseVal
        LogicOr -> genJumpIRsAndLabel varId falseVal trueVal
        _ -> pure [IRBinary op irValFromLExpr irValFromRExpr $ IRVar $ show varId]
  pure (concat
    [irsFromLExpr, lExprCondJumpIRs, irsFromRExpr, rExprCondJumpIRs, resultIRVal],
    IRVar $ show varId)

exprToIRs :: Expr -> State (Int, Int) ([IRInstruction], IRVal)
exprToIRs expr =
  case expr of
    Constant s -> pure ([], IRConstant s)
    Unary op uExpr -> unaryOperationToIRs op uExpr
    Binary op lExpr rExpr -> binaryOperationToIRs op lExpr rExpr
    _ -> undefined
