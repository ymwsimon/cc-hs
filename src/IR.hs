-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/04/03 12:51:42 by mayeung          ###   ########.fr       --
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
  deriving (Show, Eq)

data IRVal =
  IRConstant String
  | IRVar String
  deriving (Show, Eq)

exprToIRList :: Int -> Expr -> [IRInstruction] -> (Int, [IRInstruction], IRVal)
exprToIRList i expr irs = case expr of
  Constant s -> (i, irs, IRConstant s)
  Unary op uexpr ->
    let (newi, oldIRList, irVal) = exprToIRList i uexpr irs in
      (newi + 1,
        oldIRList ++ [IRUnary op irVal (IRVar $ show newi)],
        IRVar $ show newi)
  Binary op lExpr rExpr ->
    let (newiFromL, oldIRListFromL, irValFromL) =
          exprToIRList i lExpr irs 
        (newiFromR, oldIRListFromR, irValFromR) =
          exprToIRList newiFromL rExpr oldIRListFromL in
      (newiFromR + 1,
        oldIRListFromR ++ [IRBinary
                            op
                            irValFromL
                            irValFromR
                            (IRVar $ show newiFromR)],
        IRVar $ show newiFromR)
  _ -> undefined
  
addReturnToIRList :: (a, [IRInstruction], IRVal) -> [IRInstruction]
addReturnToIRList (_, irs, irVal) = irs ++ [IRReturn irVal]

cReturnStatmentToIRList :: Expr -> [IRInstruction]
cReturnStatmentToIRList expr = addReturnToIRList $ exprToIRList 1 expr []

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
