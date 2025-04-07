-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/04/07 21:46:59 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module IR where

import Parser
import Operation

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
  | IRJumpIfZero {irJumpZeroTarget :: String}
  | IRJumpIfNotZero {irJumpNZeroTarget :: String}
  | Label {labelName :: String}
  deriving (Show, Eq)

data IRVal =
  IRConstant String
  | IRVar String
  deriving (Show, Eq)

exprToIRList :: (Int, Int) -> Expr -> [IRInstruction] -> ((Int, Int), [IRInstruction], IRVal)
exprToIRList (varI, labelI) expr irs = case expr of
  Constant s -> ((varI, labelI), irs, IRConstant s)
  Unary op uexpr ->
    let ((newVarI, newLableI), oldIRList, irVal) = exprToIRList (varI, labelI) uexpr irs in
      ((newVarI + 1, newLableI),
        oldIRList ++ [IRUnary op irVal (IRVar $ show newVarI)],
        IRVar $ show newVarI)
  Binary op lExpr rExpr ->
    let ((newVarIFromL, newLabelIFromL), oldIRListFromL, irValFromL) =
          exprToIRList (varI, labelI) lExpr irs
        (appendCondJump, newLabelIFromCondJump) =
          case op of
            LogicAnd -> (oldIRListFromL ++ [IRJump $ show newLabelIFromL], newLabelIFromL + 1)
            _ -> (oldIRListFromL, newLabelIFromL)
        ((newVarIFromR, newLableIFromR), oldIRListFromR, irValFromR) =
          exprToIRList (newVarIFromL, newLabelIFromCondJump) rExpr appendCondJump in
            ((newVarIFromR + 1, newLableIFromR),
              oldIRListFromR ++ [IRBinary
                                  op
                                  irValFromL
                                  irValFromR
                                  (IRVar $ show newVarIFromR)],
              IRVar $ show newVarIFromR)
  _ -> undefined
  
addReturnToIRList :: (a, [IRInstruction], IRVal) -> [IRInstruction]
addReturnToIRList (_, irs, irVal) = irs ++ [IRReturn irVal]

cReturnStatmentToIRList :: Expr -> [IRInstruction]
cReturnStatmentToIRList expr = addReturnToIRList $ exprToIRList (1, 1) expr []

cStatmentToIRInstructions :: Statment -> [IRInstruction]
cStatmentToIRInstructions (Return expr) = cReturnStatmentToIRList expr
cStatmentToIRInstructions _ = []

cFuncDefineToIRFuncDefine :: FunctionDefine -> IRFunctionDefine
cFuncDefineToIRFuncDefine fd =
  IRFunctionDefine 
    (funName fd)
    (concatMap cStatmentToIRInstructions $ body fd)

cASTToIrAST :: CProgramAST -> IRProgramAST
cASTToIrAST = map cFuncDefineToIRFuncDefine

newaddReturnToIRList :: ((Int, Int), [IRInstruction], IRVal) -> (Int, Int, [IRInstruction])
newaddReturnToIRList ((varI, labelI), irs, irVal) = (varI, labelI, irs ++ [IRReturn irVal])

newcReturnStatmentToIRList :: Int -> Int -> Expr -> [IRInstruction] -> (Int, Int, [IRInstruction])
newcReturnStatmentToIRList varI labelI expr irs = newaddReturnToIRList $ exprToIRList (varI, labelI) expr irs

newcStatmentToIRInstructions :: (Int, Int, [IRInstruction]) -> Statment -> (Int, Int, [IRInstruction])
newcStatmentToIRInstructions (varI, labelI, irs) (Return expr) = newcReturnStatmentToIRList varI labelI expr irs
newcStatmentToIRInstructions (varI, labelI, irs) _ = (varI, labelI, irs)

newcFuncDefineToIRFuncDefine :: (Int, [IRFunctionDefine]) -> FunctionDefine -> (Int, [IRFunctionDefine])
newcFuncDefineToIRFuncDefine (labelI, irs) fd =
  let (_, newLabelI, newIRs) = foldl newcStatmentToIRInstructions (1, labelI, []) (body fd)in
    (newLabelI, irs ++ [IRFunctionDefine (funName fd) newIRs])

newcToIR :: CProgramAST -> IRProgramAST
newcToIR = snd . foldl newcFuncDefineToIRFuncDefine (1, [])
