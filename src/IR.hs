-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   IR.hs                                              :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:38:13 by mayeung           #+#    #+#             --
--   Updated: 2025/09/16 15:14:57 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module IR where

import Parser
import Operation
import Control.Monad.State
import qualified Data.Map.Strict as M
import Control.Monad
import Data.Maybe

type IRProgramAST = [IRTopLevel]

data IRTopLevel =
  IRFunc {irFuncD :: IRFunctionDefine}
  | IRStaticVar {irStaticVarD :: IRStaticVarDefine}
  | IRStaticConstant {irStaticConstD :: IRStaticConstDefine}
  deriving (Show, Eq)

data IRFunctionDefine =
  IRFunctionDefine
  {
    irFuncName :: String,
    irFuncGlobal :: Bool,
    irParameter :: [(DT, String)],
    irInstruction :: [IRInstruction],
    irFuncStackVarsSize :: [Int]
  }
  deriving (Show, Eq)

data IRStaticVarDefine =
  IRStaticVarDefine {irVarName :: String, irVarGlobal :: Bool,
    irType :: DT, irVarInitVal :: [StaticInit]}
  deriving (Show, Eq)

data IRStaticConstDefine =
  IRStaticConstDefine {irConstName :: String, irConstType :: DT, irConstInit :: StaticInit}
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
  | IRGetAddress {irAddrSrc :: IRVal, irAddrDst :: IRVal}
  | IRLoad {irLoadSrcPtr :: IRVal, irLoadDst :: IRVal}
  | IRStore {irStoreSrc :: IRVal, irStorePtrDst :: IRVal}
  | IRJump String
  | IRJumpIfZero IRVal String String
  | IRJumpIfNotZero IRVal String
  | IRJumpIfP String
  | IRJumpIfNP String
  | IRLabel {labelName :: String}
  | IRFuncCall {irFName :: String, arg :: [IRVal], irFCallDst :: IRVal}
  | IRAddPtr {irAddPtr :: IRVal, irPtrIndex :: IRVal, irPtrScale :: Int, irAddPtrDst :: IRVal}
  | IRCopyToOffset {irCopyOffsetSrc :: IRVal, irCopyOffsetDst :: String, irCopyOffset :: Int}
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
  | ZeroInit Int
  | CharInit NumConst
  | UCharInit NumConst
  | StringInit {stringInitContent :: String, stringInitZeroEnd :: Bool}
  | PointerInit String Int
  deriving (Show, Eq)

isIRFuncDefine :: IRTopLevel -> Bool
isIRFuncDefine (IRFunc _) = True
isIRFuncDefine _ = False

isIRStaticVarDefine :: IRTopLevel -> Bool
isIRStaticVarDefine (IRStaticVar _) = True
isIRStaticVarDefine _ = False

isStringInit :: StaticInit -> Bool
isStringInit (StringInit {}) = True
isStringInit _ = False

isIRConstant :: IRVal -> Bool
isIRConstant (IRConstant {}) = True
isIRConstant _ = False

irConstantToInt :: IRVal -> Int
irConstantToInt (IRConstant _ n) = numConstToInt n
irConstantToInt _ = error "not a irConstant "

irValToDT :: IRVal -> DT
irValToDT irVal = case irVal of
  IRConstant dt _ -> dt
  IRVar dt _ -> dt

dtToByteSize :: DT -> Int
dtToByteSize dt = case dt of
  DTInternal TChar -> 1
  DTInternal TSChar -> 1
  DTInternal TUChar -> 1
  DTInternal TInt -> 4
  DTInternal TUInt -> 4
  DTInternal TLong -> 8
  DTInternal TULong -> 8
  DTInternal TDouble -> 8
  DTPointer _ -> 8
  DTArray aDT (Just s) -> s * dtToByteSize aDT
  _ -> error "unknown data type to asm size"

cStatmentToIRInstructions :: BlockItem -> State ([DT], Int) [IRInstruction]
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
    _ -> error "incorrect label combination for do while"
  S (While condition bl l) -> case l of
    LoopLabel jLabel -> whileToIRs condition bl jLabel
    _ -> error "incorrect label combination for while"
  S (For forInit condition post bl l) -> case l of
    LoopLabel jLabel -> forToIRs forInit condition post bl jLabel
    _ -> error "incorrect label combination for forloop"
  S (Switch condition bl l) -> case l of
    SwitchLabel jLabel caseMap -> switchToIRs condition bl jLabel caseMap
    _ -> error "incorrect label combination for switch"
  S (Case statement l) -> caseToIRs statement l
  S (Default statement l) -> defaultToIRs statement l
  D (VariableDeclaration (VarTypeInfo var dt (Just expr) Nothing False)) ->
    varInitToIRs expr var dt 0
  D _ -> pure []

initDTList :: a1 -> (a2, b) -> (a1, b)
initDTList s (_, b) = (s, b)

hasFuncBody :: Declaration -> Bool
hasFuncBody (FunctionDeclaration (FuncTypeInfo _ _ (Just _) _ _)) = True
hasFuncBody _ = False

alignTo16 :: Integral a => a -> a
alignTo16 n = div ((-1) * abs n) 16 * 16

cFuncDefineToIRFuncDefine :: Declaration -> [DT] -> State ([DT], Int) IRTopLevel
cFuncDefineToIRFuncDefine (FunctionDeclaration fd@(FuncTypeInfo _ _ (Just bl) sc _)) dtList = do
  modify $ initDTList dtList
  res <- concat <$> mapM cStatmentToIRInstructions (unBlock bl)
  sVar <- gets $ drop 1 . map ((\s -> if s > 16 then abs $ alignTo16 s else s) . getDTSize) . fst
  pure $ IRFunc $ IRFunctionDefine (funcName fd) (sc /= Just Static) (argList $ funcType fd)
     (res ++ [IRReturn (IRConstant (DTInternal TInt) $ ConstInt 0)]) sVar
cFuncDefineToIRFuncDefine _ _ = error "function define only, not for variable define"

cASTToIrAST :: CProgramAST -> IRProgramAST
cASTToIrAST =
  flip evalState ([], 1) .
    mapM (\d -> cFuncDefineToIRFuncDefine d (DTInternal TVoid : getStackVarsDeclare d)) . filter hasFuncBody

exprToStaticInit :: TypedExpr -> StaticInit
exprToStaticInit (TExpr e _) = case e of
  Constant (ConstChar c) -> CharInit $ ConstChar c
  Constant (ConstUChar uc) -> UCharInit $ ConstUChar uc
  Constant (ConstInt i) -> IntInit $ ConstInt i
  Constant (ConstLong l) -> LongInit $ ConstLong l
  Constant (ConstUInt ui) -> UIntInit $ ConstUInt ui
  Constant (ConstULong ul) -> ULongInit $ ConstULong ul
  Constant (ConstDouble d) -> DoubleInit $ ConstDouble d
  StringLit _ strContent -> StringInit strContent True
  AddrOf (TExpr str _) -> PointerInit (strLitName str) 0
  _ -> error "unsupported expression convert to static init"

initialiserToStaticInits :: DT -> Initialiser -> [StaticInit]
initialiserToStaticInits dt i = case i of
  SingleInit si -> let sInit = exprToStaticInit si in
    case sInit of
      StringInit strContent _ -> if fromJust (arrSize dt) > length strContent
        then
          [sInit, ZeroInit (fromJust (arrSize dt) - length strContent)]
        else [sInit]
      _ -> [sInit]
  CompoundInit ci -> concatMap (initialiserToStaticInits (getRefType dt)) ci ++
    [ZeroInit $ getDTSize (getRefType dt) * (fromJust (arrSize dt) - length ci) | length ci < fromJust (arrSize dt)]

staticInitToInt :: StaticInit -> Integer
staticInitToInt (CharInit (ConstChar c)) = c
staticInitToInt (UCharInit (ConstUChar uc)) = uc
staticInitToInt (IntInit (ConstInt i)) = i
staticInitToInt (UIntInit (ConstUInt ui)) = ui
staticInitToInt (LongInit (ConstLong l)) = l
staticInitToInt (ULongInit (ConstULong ul)) = ul
staticInitToInt (DoubleInit (ConstDouble d)) = truncate d
staticInitToInt (ZeroInit z) = fromIntegral z
staticInitToInt _ = error "not a static init"

staticInitToDouble :: StaticInit -> Double
staticInitToDouble (CharInit (ConstChar c)) = fromIntegral c
staticInitToDouble (UCharInit (ConstUChar uc)) = fromIntegral uc
staticInitToDouble (IntInit (ConstInt i)) = fromIntegral i
staticInitToDouble (UIntInit (ConstUInt ui)) = fromIntegral ui
staticInitToDouble (LongInit (ConstLong l)) = fromIntegral l
staticInitToDouble (ULongInit (ConstULong ul)) = fromIntegral ul
staticInitToDouble (DoubleInit (ConstDouble d)) = d
staticInitToDouble (ZeroInit z) = fromIntegral z
staticInitToDouble _ = error "not a static init"

staticVarConversion :: M.Map String IdentifierType -> [IRTopLevel]
staticVarConversion m = map (IRStaticVar . identToStaticVar . snd) $ 
  M.toList $ M.filter (\i -> isVarIdentifier i && not (isExternIdentifier i)) m
  where identToStaticVar ident = let vInfo = vti ident in
          IRStaticVarDefine (varName vInfo) (varStoreClass vInfo /= Just Static && topLv vInfo)
            (variableType vInfo) (maybe [ZeroInit (getDTSize $ variableType vInfo)] (initialiserToStaticInits (variableType vInfo)) (varDefine vInfo))

staticConstantConversion :: M.Map String IdentifierType -> [IRTopLevel]
staticConstantConversion m = map (staticConstantCvt . snd) $
  M.toList $ M.filter (\i -> isStaticConstIdentifier i && not (isExternIdentifier i)) m
  where staticConstantCvt ident = let scInfo = scti ident in
          IRStaticConstant $ IRStaticConstDefine (constName scInfo) (constType scInfo)
            (exprToStaticInit $ constDefine scInfo)

bumpOneToVarId :: Num a => (a, b) -> (a, b)
bumpOneToVarId (a, b) = (a + 1, b)

addDTAtEnd :: a -> ([a], b) -> ([a], b)
addDTAtEnd dt (dtList, b) = (dtList ++ [dt], b)

bumpOneToLabelId :: Num b => (a, b) -> (a, b)
bumpOneToLabelId (a, b) = (a, b + 1)

exprToReturnIRs :: TypedExpr -> State ([DT], Int) [IRInstruction]
exprToReturnIRs expr = do
  (irs, irVal) <- exprToIRs expr
  pure $ irs ++ [IRReturn irVal]

exprToExpressionIRs :: TypedExpr -> State ([DT], Int) [IRInstruction]
exprToExpressionIRs expr = fst <$> exprToIRs expr

exprToIfIRs :: TypedExpr -> Statement -> Maybe Statement -> State ([DT], Int) [IRInstruction]
exprToIfIRs condition tStat fStat = do
  (cIRs, cValIR) <- exprToIRs condition
  tStatIRs <- cStatmentToIRInstructions $ S tStat
  tLabel <- if isFloatDT $ tDT condition
    then gets (("ifTrue" ++) . show . snd) <* modify bumpOneToLabelId
    else pure ""
  (fStatIRs, fLabel) <- do
    lId <- gets (show . snd) <* modify bumpOneToLabelId
    case fStat of
      Just fs -> do
        fsIRs <- cStatmentToIRInstructions $ S fs
        dLabelId <- gets (show . snd) <* modify bumpOneToLabelId
        pure ([IRJump $ "ifSkip" ++ dLabelId, IRLabel ("ifFalse" ++ lId)] ++
          fsIRs ++ [IRLabel $ "ifSkip" ++ dLabelId], "ifFalse" ++ lId)
      _ -> pure ([IRLabel ("ifSkip" ++ lId)], "ifSkip" ++ lId)
  pure $ cIRs ++
    [IRJumpIfZero cValIR fLabel tLabel] ++
    [IRLabel tLabel | tLabel /= ""] ++
    tStatIRs ++ fStatIRs

postPrefixToBin :: UnaryOp -> BinaryOp
postPrefixToBin op
  | op `elem` [PostDecrement, PreDecrement] = Minus
  | otherwise = Plus

unaryOperationToIRs :: UnaryOp -> DT -> TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
unaryOperationToIRs op dt uExpr
  | op `elem` [PostDecrement, PostIncrement] = do
    oldVal <- gets $ IRVar dt . ("#" ++) . show . length . fst
    modify $ addDTAtEnd dt
    (newValIRs, irNewVal) <-
      let c = if isFloatDT dt then makeConstantTEFloatWithDT 1.0 dt else makeConstantTEIntWithDT 1 dt in
      exprToIRs $ TExpr
        (Binary (postPrefixToBin op) uExpr c) $ tDT uExpr
    (varIRs, irVar) <- exprToIRs uExpr
    updateVarIRs <- case uExpr of
      TExpr (Dereference ptr) _ -> do
        (ptrIRs, ptrIRVar) <- exprToIRs ptr
        pure $ ptrIRs ++ [IRStore irNewVal ptrIRVar]
      _ -> pure [IRCopy irNewVal irVar]
    pure (concat [varIRs, [IRCopy irVar oldVal], newValIRs,
      updateVarIRs], oldVal)
  | op `elem` [PreDecrement, PreIncrement] = do
    let c = if isFloatDT dt then makeConstantTEFloatWithDT 1.0 dt else makeConstantTEIntWithDT 1 dt
    case uExpr of
      TExpr (Dereference ptr) _ -> do
        (ptrIRs, ptrIRVal) <- exprToIRs ptr
        oldIRVal <- gets (IRVar (getPointingType $ tDT ptr) . ("#" ++) . show . length . fst) <*
          modify (addDTAtEnd (getPointingType $ tDT ptr))
        newIRVal <- gets (IRVar (getPointingType $ tDT ptr) . ("#" ++) . show . length . fst) <*
          modify (addDTAtEnd (getPointingType $ tDT ptr))
        stepVal <- snd <$> exprToIRs c
        let loadIRs = [IRLoad ptrIRVal oldIRVal]
        let binIRs = [IRBinary (postPrefixToBin op) oldIRVal stepVal newIRVal]
        pure (ptrIRs ++ loadIRs ++ binIRs ++ [IRStore newIRVal ptrIRVal], newIRVal)
      _ -> do
        (varIRs, irVar) <- exprToIRs uExpr
        (updateIRs, newVal) <- exprToIRs $ TExpr (Binary (postPrefixToBin op) uExpr c) $ tDT uExpr
        pure (varIRs ++ updateIRs ++ [IRCopy newVal irVar], newVal)
  | otherwise = do
      (oldIRs, irVal) <- exprToIRs uExpr
      varId <- gets $ ("#" ++) . show . length . fst
      modify $ addDTAtEnd dt
      pure (oldIRs ++ [IRUnary op irVal $ IRVar dt varId], IRVar dt varId)

genJumpIRsIfNeeded :: BinaryOp -> (Int, Int, Int) -> IRVal -> State ([DT], Int) [IRInstruction]
genJumpIRsIfNeeded op (fId, tId, _) irVal = case op of
    LogicAnd ->
      pure [IRJumpIfZero irVal ("false_label" ++ show fId) ("true_label" ++ show tId)]
    LogicOr ->
      pure [IRJumpIfNotZero irVal $ "false_label" ++ show fId]
    _ -> pure []

genJumpIRsAndLabel :: String -> (Int, Int, Int) -> IRVal -> IRVal -> State ([DT], Int) [IRInstruction]
genJumpIRsAndLabel varId (fId, _, eId) fstVal sndVal = do
  pure [IRCopy fstVal $ IRVar (DTInternal TInt) varId,
    IRJump $ "end_label" ++ show eId,
    IRLabel $ "false_label" ++ show fId,
    IRCopy sndVal $ IRVar (DTInternal TInt) varId,
    IRLabel $ "end_label" ++ show eId]

genLabelIfNeeded :: BinaryOp -> State ([DT], Int) (Int, Int, Int)
genLabelIfNeeded op =
  let getLabels = do
          fId <- gets snd
          modify bumpOneToLabelId
          tId <- gets snd
          modify bumpOneToLabelId
          endLabelId <- gets snd
          modify bumpOneToLabelId
          pure (fId, tId, endLabelId) in
  case op of
    LogicAnd -> getLabels
    LogicOr -> getLabels
    _ -> pure (-1, -1, -1)

binaryOperationToIRs :: BinaryOp -> DT -> TypedExpr -> TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
binaryOperationToIRs op dt lExpr rExpr = do
  ids@(_, tId, _) <- genLabelIfNeeded op
  (irsFromLExpr, irValFromLExpr) <- exprToIRs lExpr
  lExprCondJumpIRs <- genJumpIRsIfNeeded op ids irValFromLExpr
  (irsFromRExpr, irValFromRExpr) <- exprToIRs rExpr
  rExprCondJumpIRs <- genJumpIRsIfNeeded op ids irValFromRExpr
  varId <- gets $ ("#" ++) . show . length . fst
  modify $ addDTAtEnd dt
  if op `elem` [Plus, Minus] && (isPointerOrArray lExpr || isPointerOrArray rExpr)
    then
      if op == Minus && isPointerOrArray lExpr && isPointerOrArray rExpr
        then do
          divVarId <- gets $ ("#" ++) . show . length . fst
          modify $ addDTAtEnd $ DTInternal TULong
          let refDataSize = getDTSize $ getRefType $ irVarDT irValFromLExpr
          let minusVar = IRVar (irVarDT irValFromLExpr) varId
          let minusInstr = [IRBinary Minus irValFromLExpr irValFromRExpr minusVar]
          let divVar = IRVar (irVarDT irValFromLExpr) divVarId
          let divInstr = [IRBinary Division minusVar
                (IRConstant (DTInternal TLong) (ConstULong $ fromIntegral refDataSize)) divVar]
          pure (concat
            [irsFromLExpr, irsFromRExpr, minusInstr, divInstr],
            divVar)
        else do
          let ptr = if isPointerOrArray lExpr then irValFromLExpr else irValFromRExpr
          let idx = if isPointerOrArray lExpr then irValFromRExpr else irValFromLExpr
          let dst = IRVar dt varId
          (maybeNegate, finalIdx) <- case op of
            Plus -> pure ([], idx)
            Minus -> do
              negateVarId <- gets $ ("#" ++) . show . length . fst
              modify $ addDTAtEnd dt
              let idxDst = IRVar (irValToDT idx) negateVarId
              pure ([IRUnary Negate idx idxDst], idxDst)
            _ -> error "unsupported binary operation for 2 pointers"
          let addPtrInstr = [IRAddPtr ptr finalIdx (getDTSize $ getRefType $
                (if isAddrOf lExpr || isAddrOf rExpr then getRefType else id) $ irVarDT ptr) dst]
          pure (concat [irsFromLExpr, irsFromRExpr, maybeNegate, addPtrInstr], dst)
    else do
      resultIRVal <-
        let trueVal = IRConstant (DTInternal TInt) $ ConstInt 1
            falseVal = IRConstant (DTInternal TInt) $ ConstInt 0 in
          case op of
            LogicAnd -> (IRLabel ("true_label" ++ show tId) :) <$> genJumpIRsAndLabel varId ids trueVal falseVal
            LogicOr -> (IRLabel ("true_label" ++ show tId) :) <$> genJumpIRsAndLabel varId ids falseVal trueVal
            _ -> pure [IRBinary op irValFromLExpr irValFromRExpr $ IRVar dt varId]
      pure (concat
        [irsFromLExpr, lExprCondJumpIRs, irsFromRExpr, rExprCondJumpIRs, resultIRVal],
        IRVar dt varId)

dropVarName :: String -> String
dropVarName v = if '#' `elem` v then drop 1 $ dropWhile (/= '#') v else v

isStackVar :: String -> Bool
isStackVar = elem '#'

assignmentToIRs :: CompoundAssignOp -> TypedExpr -> TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
assignmentToIRs op var rExpr = do
  let cType = getExprsCommonType var rExpr
  let dt
        | op `elem` [BitShiftLeftAssign, BitShiftRightAssign, AssignOp] = tDT var
        | otherwise = cType
  case var of
    TExpr (Dereference ptr) lDT -> do
      tempVarId <- gets (('#' :) . show . length . fst) <* modify (addDTAtEnd dt)
      (tempVarIRs, tempVar) <- if op == AssignOp
        then do
          (rIRs, rVal) <- exprToIRs $ foldToConstExpr $ (`cvtTypedExpr` dt) rExpr
          (varIRs, irVar) <- derefAssignmentToIRs ptr
          pure (rIRs ++ varIRs ++ [IRStore rVal irVar], rVal)
        else exprToIRs $ TExpr 
          (Assignment AssignOp (TExpr (Variable tempVarId False Nothing) (tDT ptr)) ptr)
          (tDT ptr)
      (newValIRs, newVal) <- if op == AssignOp
        then pure ([], tempVar)
        else do
          (binOpIRs, binOpRes) <- exprToIRs $ (`cvtTypedExpr` tDT var) $ TExpr (Binary
            (compoundAssignOpToBinOp op)
            (cvtTypedExpr (TExpr (Dereference (TExpr (Variable tempVarId False Nothing) (tDT ptr))) lDT) dt)
            ((if isPointerDT (getRefType $ irValToDT tempVar) then id else (`cvtTypedExpr` dt)) rExpr)) dt
          pure (binOpIRs ++ [IRStore binOpRes tempVar], binOpRes)
      pure (tempVarIRs ++ newValIRs, newVal)
    _ -> do
      (varIRs, irVar) <- exprToIRs var
      (newValIRs, newVal) <- case op of
        AssignOp -> exprToIRs $ foldToConstExpr $ (`cvtTypedExpr` tDT var) rExpr
        _ -> do
          if isPointerTypedExpr var
            then do
              (idxIRs, idx) <- exprToIRs rExpr
              dst <- gets (IRVar dt . ('#' :) . show . length . fst) <* modify (addDTAtEnd dt)
              (maybeNegate, finalIdx) <- case op of
                PlusAssign -> pure ([], idx)
                MinusAssign -> do
                  negateVarId <- gets $ ("#" ++) . show . length . fst
                  modify $ addDTAtEnd dt
                  let idxDst = IRVar (irValToDT idx) negateVarId
                  pure ([IRUnary Negate idx idxDst], idxDst)
                _ -> error "unsupported binary operation for 2 pointers"
              let addPtrInstr = [IRAddPtr irVar finalIdx (getDTSize $ getRefType $ irVarDT irVar) dst]
              pure (idxIRs ++ maybeNegate ++ addPtrInstr, dst)
            else exprToIRs $ (`cvtTypedExpr` tDT var) $ TExpr (Binary
              (compoundAssignOpToBinOp op)
              (cvtTypedExpr var dt)
              (cvtTypedExpr rExpr dt)) dt
      pure (newValIRs ++ varIRs ++ [IRCopy newVal irVar], irVar)

conditionToIRs :: DT -> TypedExpr -> TypedExpr -> TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
conditionToIRs dt condition tExpr fExpr = do
  (cIRs, cValIR) <- exprToIRs condition
  (tIRs, tValIR) <- exprToIRs tExpr
  (fIRs, fValIR) <- exprToIRs fExpr
  resValIR <- gets (IRVar dt . ("#" ++) . show . length . fst) <* modify (addDTAtEnd dt)
  tLabel <- if isFloatDT $ tDT condition
    then gets (("condT" ++) . show . snd) <* modify bumpOneToLabelId
    else pure ""
  fLabel <- gets (show . snd) <* modify bumpOneToLabelId
  dLabel <- gets (show . snd) <* modify bumpOneToLabelId
  pure (cIRs ++
    [IRJumpIfZero cValIR ("condF" ++ fLabel) tLabel] ++
    [IRLabel tLabel | tLabel /= ""] ++
    tIRs ++ [IRCopy tValIR resValIR, IRJump $ "condD" ++ dLabel , IRLabel $ "condF" ++ fLabel] ++
    fIRs ++ [IRCopy fValIR resValIR, IRLabel $ "condD" ++ dLabel], resValIR)

doWhileToIRs :: Statement -> TypedExpr -> (String, String, String) -> State ([DT], Int) [IRInstruction]
doWhileToIRs bl condition (sLabel, cLabel, dLabel) = do
  blIRs <- cStatmentToIRInstructions $ S bl
  (exprIRs, exprIRVal) <- exprToIRs condition
  tLabel <- if isFloatDT $ tDT condition
    then gets (pure . show . snd) <* modify bumpOneToLabelId
    else pure []
  pure $ IRLabel sLabel : [IRLabel $ "ifTrue" ++ (!! 0) tLabel | not (null tLabel)] ++
    blIRs ++ [IRLabel cLabel] ++
    exprIRs ++ [IRJumpIfP $ "ifTrue" ++ (!! 0) tLabel | not (null tLabel)] ++
    [IRJumpIfNotZero exprIRVal sLabel, IRLabel dLabel]

whileToIRs :: TypedExpr -> Statement -> (String, String, String) -> State ([DT], Int) [IRInstruction]
whileToIRs condition bl (_, cLabel, dLabel) = do
  (exprIRs, exprIRVal) <- exprToIRs condition
  tLabel <- if isFloatDT $ tDT condition
    then gets (("whileTrue" ++) . show . snd) <* modify bumpOneToLabelId
    else pure ""
  blIRs <- cStatmentToIRInstructions $ S bl
  pure $ IRLabel cLabel : exprIRs ++
    [IRJumpIfZero exprIRVal dLabel tLabel] ++
    [IRLabel tLabel | tLabel /= ""] ++
    blIRs ++ [IRJump cLabel, IRLabel dLabel]

forInitToIRs :: ForInit -> State ([DT], Int) [IRInstruction]
forInitToIRs fi = case fi of
  InitDecl d -> cStatmentToIRInstructions $ D $ VariableDeclaration d
  InitExpr (Just expr) -> cStatmentToIRInstructions $ S $ Expression expr
  _ -> pure []

forToIRs :: ForInit -> Maybe TypedExpr -> Maybe TypedExpr -> Statement ->
  (String, String, String) -> State ([DT], Int) [IRInstruction]
forToIRs forInit condition post bl (sLabel, cLabel, dLabel) = do
  forInitIRs <- forInitToIRs forInit
  conditionIRs <- case condition of
    Just c -> do
      (exprIRs, exprIRVal) <- exprToIRs c
      tLabel <- if isFloatDT $ tDT c
        then gets (("forTrue" ++) . show . snd) <* modify bumpOneToLabelId
        else pure ""
      pure $ IRLabel sLabel : exprIRs ++
        [IRJumpIfZero exprIRVal dLabel tLabel] ++
        [IRLabel tLabel | tLabel /= ""]
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
          let irConst = if irValToDT irVal == DTInternal TLong
                          then IRConstant (DTInternal TLong) $ ConstLong val
                          else IRConstant (DTInternal TInt) $ ConstInt $ fromIntegral val in
          [IRBinary EqualRelation irVal irConst resIRVal,
            IRJumpIfNotZero resIRVal l]

switchToIRs :: TypedExpr -> Statement -> (Maybe String, String) ->
  M.Map Integer String -> State ([DT], Int) [IRInstruction]
switchToIRs c@(TExpr _ cDt) bl (defaultLabel, doneLabel) caseMap = do
  (exprIRs, exprIRVal) <- exprToIRs c
  varId <- gets (("#" ++) . show . length . fst) <* modify (addDTAtEnd cDt)
  blIRs <- cStatmentToIRInstructions $ S bl
  defaultIRs <- case defaultLabel of
    Just l -> pure [IRJump l]
    _ -> pure []
  let caseIRs = caseMapToIRJump exprIRVal (IRVar (DTInternal TInt) varId) caseMap
  pure $ exprIRs ++ caseIRs ++ defaultIRs ++ [IRJump doneLabel] ++ blIRs ++ [IRLabel doneLabel]

caseToIRs :: Statement -> String -> State ([DT], Int) [IRInstruction]
caseToIRs statement l = do
  stateIRs <- cStatmentToIRInstructions $ S statement
  pure $ IRLabel l : stateIRs

defaultToIRs :: Statement -> String -> State ([DT], Int) [IRInstruction]
defaultToIRs statement l = do
  stateIRs <- cStatmentToIRInstructions $ S statement
  pure $ IRLabel l : stateIRs

funcCallToIRs :: String -> DT -> [TypedExpr] -> State ([DT], Int) ([IRInstruction], IRVal)
funcCallToIRs name dt exprs = do
  varId <- gets $ IRVar dt . ("#" ++) . show . length . fst
  modify $ addDTAtEnd dt
  (irs, irVal) <- mapAndUnzipM exprToIRs exprs
  pure (concat irs ++ [IRFuncCall name irVal varId], varId)

truncateIRVal :: IRVal -> DT -> IRVal
truncateIRVal irVal dt = case irVal of
  og@(IRConstant _ (ConstLong l)) -> case dt of
    DTInternal TInt -> IRConstant dt (ConstInt (fromIntegral l))
    DTInternal TLong -> og
    _ -> error "unsupported ir value truncation"
  _ -> irVal

castToIRs :: DT -> TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
castToIRs dt te@(TExpr _ tdt)
  | dt == tdt = exprToIRs te
  | otherwise = do
    (teIRs, teIRVal) <- exprToIRs te
    varId <- gets (IRVar dt . ("#" ++) . show . length . fst) <* modify (addDTAtEnd dt)
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

derefToIRs :: TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
derefToIRs ptr = do
  (ptrIRs, ptrIRVal) <- exprToIRs ptr
  resIRVal <- gets (IRVar (getPointingType $ tDT ptr) . ("#" ++) . show . length . fst) <*
    modify (addDTAtEnd (getPointingType $ tDT ptr))
  pure (ptrIRs ++ [IRLoad ptrIRVal resIRVal], resIRVal)

derefAssignmentToIRs :: TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
derefAssignmentToIRs ptr = do
  (ptrIRs, ptrIRVal) <- exprToIRs ptr
  pure (ptrIRs, ptrIRVal)

addrOfToIRs :: TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
addrOfToIRs v@(TExpr var dt) =
  let varOrStringLit = do
        (varIRs, varIRVal) <- exprToIRs v
        resIRVal <- gets (IRVar (DTPointer dt) . ("#" ++) . show . length . fst) <* modify (addDTAtEnd $ DTPointer dt)
        pure (varIRs ++ [IRGetAddress varIRVal resIRVal], resIRVal) in
  case var of
  Variable {} -> varOrStringLit
  StringLit _ _ -> varOrStringLit
  Dereference ptr -> exprToIRs ptr
  _ -> error "unsupported address of operation for ir value"

subscriptToIRs :: TypedExpr -> TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
subscriptToIRs l r = do
  let ptr = if isPointerOrArray l then l else r
  let idx = if isPointerOrArray l then r else l
  (ptrIRs, ptrVal) <- exprToIRs ptr
  (idxIRs, idxVal) <- exprToIRs idx
  resIRVal <- gets (IRVar (DTPointer (getRefType $ tDT ptr)) . ("#" ++) . show . length . fst) <* modify (addDTAtEnd $ DTPointer (getRefType $ tDT ptr))
  let calPtrIRs = [IRAddPtr ptrVal idxVal (getDTSize $ getRefType $ tDT ptr) resIRVal]
  pure (ptrIRs ++ idxIRs ++ calPtrIRs, resIRVal)

exprToIRs :: TypedExpr -> State ([DT], Int) ([IRInstruction], IRVal)
exprToIRs (TExpr expr dt) = case expr of
  Constant (ConstChar s) -> pure ([], IRConstant (DTInternal TChar) $ ConstChar s)
  Constant (ConstUChar s) -> pure ([], IRConstant (DTInternal TUChar) $ ConstUChar s)
  Constant (ConstShort s) -> pure ([], IRConstant (DTInternal TShort) $ ConstShort s)
  Constant (ConstUShort s) -> pure ([], IRConstant (DTInternal TUShort) $ ConstUShort s)
  Constant (ConstInt s) -> pure ([], IRConstant (DTInternal TInt) $ ConstInt s)
  Constant (ConstUInt s) -> pure ([], IRConstant (DTInternal TUInt) $ ConstUInt s)
  Constant (ConstLong s) -> pure ([], IRConstant (DTInternal TLong) $ ConstLong s)
  Constant (ConstULong s) -> pure ([], IRConstant (DTInternal TULong) $ ConstULong s)
  Constant (ConstDouble s) -> pure ([], IRConstant (DTInternal TDouble) $ ConstDouble s)
  StringLit strName _ -> pure ([], IRVar (DTPointer (getRefType dt)) strName)
  Unary op uExpr -> unaryOperationToIRs op dt uExpr
  Binary op lExpr rExpr -> binaryOperationToIRs op dt lExpr rExpr
  Variable var _ _ -> pure ([], IRVar dt var)
  Assignment op vExpr rExpr ->
    assignmentToIRs op vExpr rExpr
  Conditional condition tCond fCond -> conditionToIRs dt condition tCond fCond
  FunctionCall name exprs -> funcCallToIRs name dt exprs
  Cast cDt cExpr -> castToIRs cDt cExpr
  Dereference ptr -> derefToIRs ptr
  AddrOf var -> addrOfToIRs var
  Subscript l r -> subscriptToIRs l r

initToIRs :: Initialiser -> IdentifierName -> DT -> Int -> State ([DT], Int) [IRInstruction]
initToIRs i n dt offset = case i of
  SingleInit si -> do
    (siIR, siIRVal) <- exprToIRs si
    pure $ siIR ++ [IRCopyToOffset siIRVal n offset]
  CompoundInit ci ->
    concat <$> zipWithM (\cii idx -> initToIRs cii n (getRefType dt) (offset + getDTSize (getRefType dt) * idx)) ci [0..]

varInitToIRs :: Initialiser -> IdentifierName -> DT -> Int -> State ([DT], Int) [IRInstruction]
varInitToIRs i vName dt offset = do
  zeroIRs <- if isArrayDT dt
    then 
      fst <$> funcCallToIRs "memset" (DTPointer dt)
        [TExpr (AddrOf $ TExpr (Variable vName False Nothing) dt) (DTPointer dt),
          TExpr (Constant (ConstInt 0)) (DTInternal TInt),
          TExpr (Constant (ConstInt $ fromIntegral $ dtToByteSize dt)) (DTInternal TInt)]
    else pure []
  initIRs <- initToIRs i vName dt offset
  pure $ zeroIRs ++ initIRs
