-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/06/25 15:54:54 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module IR where

import Parser
import Operation
import Control.Monad.State
import qualified Data.Map.Strict as M
import Control.Monad (mapAndUnzipM)
import Data.Char

type IRProgramAST = [IRFunctionDefine]

data IRFunctionDefine =
  IRFunctionDefine { irFuncName :: String, irParameter :: [String], irInstruction :: [IRInstruction] }
  deriving (Show, Eq)

data IRInstruction =
  IRReturn IRVal
  | IRUnary { irUnaryOp :: UnaryOp, irUnarySrc :: IRVal, irUnaryDst :: IRVal }
  | IRBinary { irBinaryOp :: BinaryOp, irLOperand :: IRVal,
      irROperand :: IRVal, irBinaryDst :: IRVal }
  | IRCopy { irCopySrc :: IRVal, irCopyDst :: IRVal }
  | IRJump {irJumpTarget :: String}
  | IRJumpIfZero {irJumpZVal :: IRVal, irJumpZeroTarget :: String}
  | IRJumpIfNotZero {irJumpNZVal :: IRVal, irJumpNotZeroTarget :: String}
  | IRLabel {labelName :: String}
  | IRFuncCall {irFName :: String, arg :: [IRVal], irFCallDst :: IRVal}
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
cStatmentToIRInstructions (S (Label _ l stat)) =
  (IRLabel l :) <$> cStatmentToIRInstructions (S stat)
cStatmentToIRInstructions (S (Goto l)) = pure [IRJump l]
cStatmentToIRInstructions (S (Compound (Block bl))) =
  concat <$> traverse cStatmentToIRInstructions bl
cStatmentToIRInstructions (S (Break l)) = pure [IRJump l]
cStatmentToIRInstructions (S (Continue l)) = pure [IRJump l]
cStatmentToIRInstructions (S (DoWhile bl condition (LoopLabel jLabel))) =
  doWhileToIRs bl condition jLabel
cStatmentToIRInstructions (S (While condition bl (LoopLabel jLabel))) =
  whileToIRs condition bl jLabel
cStatmentToIRInstructions (S (For forInit condition post bl (LoopLabel jLabel))) =
  forToIRs forInit condition post bl jLabel
cStatmentToIRInstructions (S (Switch condition bl (SwitchLabel jLabel caseMap))) =
  switchToIRs condition bl jLabel caseMap
cStatmentToIRInstructions (S (Case statement l)) = caseToIRs statement l
cStatmentToIRInstructions (S (Default statement l)) = defaultToIRs statement l
cStatmentToIRInstructions (D (VD ((VariableDeclaration _ var (Just expr))))) =
  cStatmentToIRInstructions (S (Expression (Assignment None (Variable var) expr)))
cStatmentToIRInstructions (D _) = pure []
cStatmentToIRInstructions _ = undefined

initIRVarId :: a1 -> (a2, b) -> (a1, b)
initIRVarId s (_, b) = (s, b)

cFuncDefineToIRFuncDefine :: Declaration -> State (Int, Int) IRFunctionDefine
cFuncDefineToIRFuncDefine fd@(FunctionDeclaration _ _ _ (Just bl) _) =
  IRFunctionDefine (funName fd) (map varName (inputArgs fd))
    . (++ [IRReturn (IRConstant "0")]) . concat
    <$> (modify (initIRVarId (nextVarId fd)) >>
      mapM cStatmentToIRInstructions (unBlock bl))
cFuncDefineToIRFuncDefine _ = undefined

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
    lId <- gets (show . snd) <* modify bumpOneToLabelId
    case fStat of
      Just fs -> do 
        fsIRs <- cStatmentToIRInstructions $ S fs
        dLabelId <- gets (show . snd) <* modify bumpOneToLabelId
        pure ([IRJump $ "ifSkip" ++ dLabelId, IRLabel ("ifFalse" ++ lId)] ++ fsIRs ++ [IRLabel $ "ifSkip" ++ dLabelId], "ifFalse" ++ lId)
      _ -> pure ([IRLabel ("ifSkip" ++ lId)], "ifSkip" ++ lId)
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
genJumpIRsIfNeeded op lId irVal =
  case op of
    LogicAnd ->
      pure [IRJumpIfZero irVal $ "false_label" ++ show (fst lId)]
    LogicOr ->
      pure [IRJumpIfNotZero irVal $ "false_label" ++ show (fst lId)]
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
          lId <- gets snd
          modify bumpOneToLabelId
          endLabelId <- gets snd
          modify bumpOneToLabelId
          pure (lId, endLabelId) in
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
dropVarName v = if '#' `elem` v then drop 1 $ dropWhile (/= '#') v else v

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

doWhileToIRs :: Statement -> Expr -> (String, String, String) -> State (Int, Int) [IRInstruction]
doWhileToIRs bl condition (sLabel, cLabel, dLabel) = do
  blIRs <- cStatmentToIRInstructions $ S bl
  (exprIRs, exprIRVal) <- exprToIRs condition
  pure $ IRLabel sLabel : blIRs ++ [IRLabel cLabel] ++
    exprIRs ++ [IRJumpIfNotZero exprIRVal sLabel, IRLabel dLabel]

whileToIRs :: Expr -> Statement -> (String, String, String) -> State (Int, Int) [IRInstruction]
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

forToIRs :: ForInit -> Maybe Expr -> Maybe Expr -> Statement -> (String, String, String) -> State (Int, Int) [IRInstruction]
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

caseMapToIRJump :: IRVal -> IRVal -> M.Map Int String -> [IRInstruction]
caseMapToIRJump irVal resIRVal m = concatMap caseToIRJump $ M.toList m
  where caseToIRJump (val, l) =
          [IRBinary EqualRelation irVal (IRConstant (show val)) resIRVal,
          IRJumpIfNotZero resIRVal l]

switchToIRs :: Expr -> Statement -> (Maybe String, String) -> M.Map Int String -> State (Int, Int) [IRInstruction]
switchToIRs condition bl (defaultLabel, doneLabel) caseMap = do
  (exprIRs, exprIRVal) <- exprToIRs condition
  varId <- gets fst <* modify bumpOneToVarId
  blIRs <- cStatmentToIRInstructions $ S bl
  defaultIRs <- case defaultLabel of
    Just l -> pure [IRJump l]
    _ -> pure []
  let caseIRs = caseMapToIRJump exprIRVal (IRVar (show varId)) caseMap
  pure $ exprIRs ++ caseIRs ++ defaultIRs ++ [IRJump doneLabel] ++ blIRs ++ [IRLabel doneLabel]

caseToIRs :: Statement -> String -> State (Int, Int) [IRInstruction]
caseToIRs statement l = do
  stateIRs <- cStatmentToIRInstructions $ S statement
  pure $ IRLabel l : stateIRs

defaultToIRs :: Statement -> String -> State (Int, Int) [IRInstruction]
defaultToIRs statement l = do
  stateIRs <- cStatmentToIRInstructions $ S statement
  pure $ IRLabel l : stateIRs

funcCallToIRs :: String -> [Expr] -> State (Int, Int) ([IRInstruction], IRVal)
funcCallToIRs name exprs = do
  varId <- gets $ IRVar . show . fst
  modify bumpOneToVarId
  (irs, irVal) <- mapAndUnzipM exprToIRs exprs
  pure (concat irs ++ [IRFuncCall name irVal varId], varId)

exprToIRs :: Expr -> State (Int, Int) ([IRInstruction], IRVal)
exprToIRs expr = case expr of
    Constant s -> pure ([], IRConstant s)
    Unary op uExpr -> unaryOperationToIRs op uExpr
    Binary op lExpr rExpr -> binaryOperationToIRs op lExpr rExpr
    Variable var -> pure ([], IRVar (dropVarName var))
    Assignment op (Variable var) rExpr -> assignmentToIRs op (Variable var) rExpr
    Conditional condition tCond fCond -> conditionToIRs condition tCond fCond
    FunctionCall name exprs -> funcCallToIRs name exprs
    _ -> undefined

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
  where getVarId i = case i of
          IRConstant _ -> 0
          IRVar iv -> if all isDigit iv then read iv else 0

getMaxStackVarId :: [IRInstruction] -> Int
getMaxStackVarId = maximum . concatMap extractVarId 
