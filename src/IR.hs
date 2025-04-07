-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/04/06 19:47:40 by mayeung          ###   ########.fr       --
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
  | Label {lableName :: String}
  deriving (Show, Eq)

data IRVal =
  IRConstant String
  | IRVar String
  deriving (Show, Eq)

exprToIRList :: (Int, Int) -> Expr -> [IRInstruction] -> ((Int, Int), [IRInstruction], IRVal)
exprToIRList (varI, lableI) expr irs = case expr of
  Constant s -> ((varI, lableI), irs, IRConstant s)
  Unary op uexpr ->
    let ((newVarI, newLableI), oldIRList, irVal) = exprToIRList (varI, lableI) uexpr irs in
      ((newVarI + 1, newLableI),
        oldIRList ++ [IRUnary op irVal (IRVar $ show newVarI)],
        IRVar $ show newVarI)
  Binary LogicAnd lExpr rExpr -> undefined
  Binary LogicOr lExpr rExpr -> undefined
  Binary op lExpr rExpr ->
    let ((newVarIFromL, newLabelIFromL), oldIRListFromL, irValFromL) =
          exprToIRList (varI, lableI) lExpr irs
        ((newVarIFromR, newLableIFromR), oldIRListFromR, irValFromR) =
          exprToIRList (newVarIFromL, newLabelIFromL) rExpr oldIRListFromL in
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
