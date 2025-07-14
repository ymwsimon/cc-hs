-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Assembly.hs                                        :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:33:35 by mayeung           #+#    #+#             --
--   Updated: 2025/07/14 09:56:06 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Assembly where

import IR
import Operation
import Data.List
import qualified Data.Map.Strict as M
import Parser

type AsmProgramAST = [AsmTopLevel]

data AsmTopLevel =
  AsmFunc {asmFuncD :: AsmFunctionDefine}
  | AsmStaticVar {asmVarD :: AsmStaticVarDefine}
  | AsmStaticConstant {asmConstD :: AsmStaticConstantDefine}
  deriving (Show, Eq)

data AsmStaticVarDefine =
  AsmStaticVarDefine {asmVarName :: String, asmVarGlobal :: Bool, asmVarAlign :: Int, asmVarInit :: StaticInit}
  deriving (Show, Eq)

data AsmStaticConstantDefine =
  AsmStaticConstantDefine {asmConstName :: String, asmConstAlign :: Int, asmConstInit :: StaticInit}
  deriving (Show, Eq)

data AsmFunctionDefine =
  AsmFunctionDefine {asmFuncName :: String, asmFuncGlobal :: Bool, instructions :: [AsmInstruction]}
  deriving (Show, Eq)

data AsmInstruction =
  Mov {asmMovType :: AsmType, asmMovSrc :: Operand, asmMovDst :: Operand}
  | Movsx {asmMovsxSrc :: Operand, asmMovsxDst :: Operand}
  | MovZeroExtend {asmMovZESrc :: Operand, asmMovZEDst :: Operand}
  | Ret
  | AsmUnary AsmUnaryOp AsmType Operand
  | AsmBinary AsmBinaryOp AsmType Operand Operand
  | AsmIdiv AsmType Operand
  | AsmDiv AsmType Operand
  | Cdq AsmType
  | AllocateStack Int
  | Cmp AsmType Operand Operand
  | AsmJmp {jumpIdentifier :: String}
  | JmpCC {cond :: CondCode, jumpCCIdentifier :: String}
  | SetCC {cond :: CondCode, setCCDst :: Operand}
  | AsmLabel {labelIdentifier :: String}
  | DeallocateStack Int
  | Push Operand
  | Call String
  deriving (Show, Eq)

data AsmType =
  AsmByte
  | AsmWord
  | LongWord
  | QuadWord
  | AsmDouble
  deriving Eq

data AsmUnaryOp =
  AsmNeg
  | AsmNot
  deriving Eq

data AsmBinaryOp =
  AsmPlus
  | AsmMius
  | AsmMul
  | AsmIDivOp
  | AsmDivOp
  | AsmMod
  | AsmBitAnd
  | AsmBitOr
  | AsmBitXor
  | AsmShiftL
  | AsmShiftR
  | AsmUShiftL
  | AsmUShiftR
  deriving Eq

data CondCode =
  A | AE
  | B | BE
  | E | NE
  | G | GE
  | L | LE
  deriving Eq

data Reg =
  AL | AX | EAX | RAX
  | BL | BX | EBX | RBX
  | CL | CX | ECX | RCX
  | DL | DX | EDX | RDX
  | SIL | SI | ESI | RSI
  | DIL | DI | EDI | RDI
  | BPL | BP | EBP | RBP
  | SPL | SP | ESP | RSP
  | R8B | R8W | R8D | R8
  | R9B | R9W | R9D | R9
  | R10B | R10W | R10D | R10
  | R11B | R11W | R11D | R11
  | R12B | R12W | R12D | R12
  | XMM0 | XMM1 | XMM2 | XMM3
  | XMM4 | XMM5 | XMM6 | XMM7
  | XMM8 | XMM9 | XMM10 | XMM11
  | XMM12 | XMM13 | XMM14 | XMM15
  deriving (Eq, Enum, Ord)

data MemorySize =
  BYTE
  | WORD
  | DWORD
  | QWORD
  deriving (Show, Eq)

data Operand =
  Imm NumConst
  | Register Reg
  | Pseudo {identifier :: String}
  | Stack Int
  | Data String
  deriving Eq

instance Show AsmType where
  show AsmByte = "byte"
  show AsmWord = "word"
  show LongWord = "long"
  show QuadWord = "quad"
  show AsmDouble = "double"

instance Show Operand where
  show (Imm n) = "$" ++ numConstToStr n
  show (Register s) = "%" ++ show s
  show (Pseudo ident) = "tmpVar." ++ show ident
  show (Stack i) = show i ++ "(%rbp)"
  show (Data s) = s ++ "(%rip)"

instance Show AsmUnaryOp where
  show AsmNeg = "neg"
  show AsmNot = "not"

instance Show AsmBinaryOp where
  show AsmPlus = "add"
  show AsmMius = "sub"
  show AsmMul = "imul"
  show AsmIDivOp = undefined
  show AsmDivOp = undefined
  show AsmMod = undefined
  show AsmBitAnd = "and"
  show AsmBitOr = "or"
  show AsmBitXor = "xor"
  show AsmShiftL = "sal"
  show AsmShiftR = "sar"
  show AsmUShiftL = "shl"
  show AsmUShiftR = "shr"

instance Show CondCode where
  show A = "a"
  show AE = "ae"
  show B = "b"
  show BE = "be"
  show E = "e"
  show NE = "ne"
  show G = "g"
  show GE = "ge"
  show L = "l"
  show LE = "le"

instance Show Reg where
  show AL = "al"
  show AX = "ax"
  show EAX = "eax"
  show RAX = "rax"
  show BL = "bl"
  show BX = "bx"
  show EBX = "ebx"
  show RBX = "rbx"
  show CL = "cl"
  show CX = "cx"
  show ECX = "ecx"
  show RCX = "rcx"
  show DL = "dl"
  show DX = "dx"
  show EDX = "edx"
  show RDX = "rdx"
  show SIL = "sil"
  show SI = "si"
  show ESI = "esi"
  show RSI = "rsi"
  show DIL = "dil"
  show DI = "di"
  show EDI = "edi"
  show RDI = "rdi"
  show BPL = "bpl"
  show BP = "bp"
  show EBP = "ebp"
  show RBP = "rbp"
  show SPL = "spl"
  show SP = "sp"
  show ESP = "esp"
  show RSP = "rsp"
  show R8B = "r8b"
  show R8W = "r8w"
  show R8D = "r8d"
  show R8 = "r8"
  show R9B = "r9b"
  show R9W = "r9w"
  show R9D = "r9d"
  show R9 = "r9"
  show R10B = "r10b"
  show R10W = "r10w"
  show R10D = "r10d"
  show R10 = "r10"
  show R11B = "r11b"
  show R11W = "r11w"
  show R11D = "r11d"
  show R11 = "r11"
  show R12B = "r12b"
  show R12W = "r12w"
  show R12D = "r12d"
  show R12 = "r12"
  show XMM0 = "xmm0"
  show XMM1 = "xmm1"
  show XMM2 = "xmm2"
  show XMM3 = "xmm3"
  show XMM4 = "xmm4"
  show XMM5 = "xmm5"
  show XMM6 = "xmm6"
  show XMM7 = "xmm7"
  show XMM8 = "xmm8"
  show XMM9 = "xmm9"
  show XMM10 = "xmm10"
  show XMM11 = "xmm11"
  show XMM12 = "xmm12"
  show XMM13 = "xmm13"
  show XMM14 = "xmm14"
  show XMM15 = "xmm15"

isAsmFuncDefine :: AsmTopLevel -> Bool
isAsmFuncDefine (AsmFunc _) = True
isAsmFuncDefine _ = False

isAsmStaticVarDefine :: AsmTopLevel -> Bool
isAsmStaticVarDefine (AsmStaticVar _) = True
isAsmStaticVarDefine _ = False

staticInitToAsmType :: StaticInit -> AsmType
staticInitToAsmType si = case si of
  ShortInit _ -> AsmWord
  UShortInit _ -> AsmWord
  IntInit _ -> LongWord
  UIntInit _ -> LongWord
  LongInit _ -> QuadWord
  ULongInit _ -> QuadWord
  DoubleInit _ -> QuadWord

staticInitToAsmSize :: StaticInit -> Int
staticInitToAsmSize si = case si of
  IntInit _ -> 4
  UIntInit _ -> 4
  LongInit _ -> 8
  ULongInit _ -> 8
  _ -> undefined

dtToAsmSize :: DT -> Int
dtToAsmSize dt = case dt of
  DTInternal TInt -> 4
  DTInternal TUInt -> 4
  DTInternal TLong -> 8
  DTInternal TULong -> 8
  _ -> undefined

dtToAsmType :: DT -> AsmType
dtToAsmType dt = case dt of
  DTInternal TInt -> LongWord
  DTInternal TUInt -> LongWord
  DTInternal TLong -> QuadWord
  DTInternal TULong -> QuadWord
  _ -> undefined

asmTypeToMemSize :: AsmType -> MemorySize
asmTypeToMemSize t = case t of
  AsmByte -> BYTE
  AsmWord -> WORD
  LongWord -> DWORD
  QuadWord -> QWORD
  _ -> undefined

irStaticVarToAsmStaticVarDefine :: IRTopLevel -> AsmStaticVarDefine
irStaticVarToAsmStaticVarDefine irD = case irD of
  IRStaticVar (IRStaticVarDefine vName global vType initVal) ->
    AsmStaticVarDefine vName global (dtToAsmSize vType) initVal
  _ -> undefined

irASTToAsmAST :: M.Map String IdentifierType -> IRProgramAST -> AsmProgramAST
irASTToAsmAST m irAst = map
  (AsmFunc . irFuncDefineToAsmFuncDefine m (getFuncList (filter isIRFuncDefine irAst)). irFuncD)
  (filter isIRFuncDefine irAst)

parametersRegister :: [Operand]
parametersRegister = map Register [EDI, ESI, EDX, ECX, R8D, R9D] ++ map Stack [16, 24..]

getPaddingSize :: [a] -> Int
getPaddingSize args = if odd $ length args then 8 else 0

getFuncList :: IRProgramAST -> M.Map String String
getFuncList = foldl' (\m fd -> M.insert (irFuncName $ irFuncD fd) (irFuncName $ irFuncD fd) m) M.empty

irFuncDefineToAsmFuncDefine :: M.Map String IdentifierType -> M.Map String String -> IRFunctionDefine -> AsmFunctionDefine
irFuncDefineToAsmFuncDefine gVarMap funcList fd = do
  let (m, instrs) = copyParametersToStack
          ((+ 1) . getMaxStackVarId $ irInstruction fd)
          (M.empty, []) (zip parametersRegister (irParameter fd))
  AsmFunctionDefine (irFuncName fd) (irFuncGlobal fd) $
    instrs ++
    [AllocateStack (getPaddingSize (drop 6 $ irParameter fd))] ++
    concatMap (\irs -> irInstructionToAsmInstruction irs m funcList gVarMap) (irInstruction fd)

irOperandToAsmOperand :: M.Map String Int -> M.Map String IdentifierType -> IRVal -> Operand
irOperandToAsmOperand _ _ (IRConstant _ n) = Imm n
irOperandToAsmOperand m gVarMap (IRVar _ s)
  | M.member s gVarMap && isVarIdentifier (gVarMap M.! s) = Data s
  | M.member (dropVarName s) m = Pseudo $ show $ m M.! dropVarName s
  | otherwise = Pseudo $ dropVarName s

irUnaryOpToAsmOp :: UnaryOp -> AsmUnaryOp
irUnaryOpToAsmOp Complement = AsmNot
irUnaryOpToAsmOp Negate = AsmNeg
irUnaryOpToAsmOp _ = undefined

irBinaryOpToAsmOp :: BinaryOp -> AsmBinaryOp
irBinaryOpToAsmOp Plus = AsmPlus
irBinaryOpToAsmOp Minus = AsmMius
irBinaryOpToAsmOp Multiply = AsmMul
irBinaryOpToAsmOp Division = AsmDivOp
irBinaryOpToAsmOp Modulo = AsmMod
irBinaryOpToAsmOp BitAnd = AsmBitAnd
irBinaryOpToAsmOp BitOr = AsmBitOr
irBinaryOpToAsmOp BitXor = AsmBitXor
irBinaryOpToAsmOp BitShiftLeft = AsmShiftL
irBinaryOpToAsmOp BitShiftRight = AsmShiftR
irBinaryOpToAsmOp _ = undefined

irFuncCallToAsm :: String -> [IRVal] -> IRVal -> M.Map String String ->
  M.Map String Int -> M.Map String IdentifierType -> [AsmInstruction]
irFuncCallToAsm name args dst funcList m gVarMap =
  let (regArg, stackArg) = splitAt 6 args
      paddingSize = getPaddingSize stackArg
      paddingInstr = [AllocateStack paddingSize]
      copyRegArgsInstr =
        zipWith (\pr a -> Mov (dtToAsmType $ irValToDT a) (cvtOperand a) pr)
          parametersRegister regArg
      copyStackArgsInstr = concatMap (\sa -> case sa of
        IRConstant _ c ->
              [Mov QuadWord (Imm c) (Register R10),
                Push (Register R10)]
        IRVar t _ ->
          [Mov (dtToAsmType t)(cvtOperand sa) (Register RAX),
            Push (Register RAX)]) (reverse stackArg)
      callInstrs = if M.member name funcList
          then [Call name] else [Call $ name ++ "@PLT"] in
  paddingInstr ++ copyRegArgsInstr ++ copyStackArgsInstr ++ callInstrs ++
    [DeallocateStack (8 * length stackArg + paddingSize)] ++
    [Mov (dtToAsmType $ irValToDT dst) (Register EAX) $ cvtOperand dst]
    where cvtOperand = irOperandToAsmOperand m gVarMap

irBinaryInstrToAsmInstr :: IRInstruction -> M.Map String Int ->
  M.Map String IdentifierType -> [AsmInstruction]
irBinaryInstrToAsmInstr instr m gVarMap = case instr of
  IRBinary Division lVal rVal d -> let op l r
                                        | isSigned (irValToDT l) || isSigned (irValToDT r) = AsmIdiv
                                        | otherwise = AsmDiv in
    [Mov (dtToAsmType $ irValToDT lVal) (cvtOperand lVal) (Register AX),
      if isSigned (irValToDT lVal) || isSigned (irValToDT rVal)
        then Cdq (dtToAsmType $ irValToDT lVal)
        else Mov (dtToAsmType $ irValToDT lVal) (Imm $ ConstInt 0) (Register DX),
      op lVal rVal (dtToAsmType $ irValToDT lVal) $ cvtOperand rVal,
      Mov (dtToAsmType $ irValToDT lVal) (Register AX) (cvtOperand d)]
  IRBinary Modulo lVal rVal d -> let op l r
                                        | isSigned (irValToDT l) || isSigned (irValToDT r) = AsmIdiv
                                        | otherwise = AsmDiv in
    [Mov (dtToAsmType $ irValToDT lVal) (cvtOperand lVal) (Register AX),
       if isSigned (irValToDT lVal) || isSigned (irValToDT rVal)
        then Cdq (dtToAsmType $ irValToDT lVal)
        else Mov (dtToAsmType $ irValToDT lVal) (Imm $ ConstInt 0) (Register DX),
      op lVal rVal (dtToAsmType $ irValToDT lVal) $ cvtOperand rVal,
      Mov (dtToAsmType $ irValToDT lVal) (Register DX) (cvtOperand d)]
  IRBinary BitShiftLeft lVal rVal d -> let op = if isSigned $ irValToDT lVal then AsmShiftL else AsmUShiftL in
    [Mov (dtToAsmType $ irValToDT lVal) (cvtOperand lVal) (Register R12),
      Mov (dtToAsmType $ irValToDT rVal) (cvtOperand rVal) (Register CX),
      AsmBinary op (dtToAsmType $ irValToDT lVal) (Register CX) (Register R12),
      Mov (dtToAsmType $ irValToDT lVal) (Register R12) $ cvtOperand d]
  IRBinary BitShiftRight lVal rVal d -> let op = if isSigned $ irValToDT lVal then AsmShiftR else AsmUShiftR in
    [Mov (dtToAsmType $ irValToDT lVal) (cvtOperand lVal) (Register R12),
      Mov (dtToAsmType $ irValToDT rVal) (cvtOperand rVal) (Register CX),
      AsmBinary op (dtToAsmType $ irValToDT lVal) (Register CX) (Register R12),
      Mov (dtToAsmType $ irValToDT lVal) (Register R12) (cvtOperand d)]
  IRBinary EqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary NotEqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary GreaterThanRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary GreaterEqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary LessThanRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary LessEqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary op lVal rVal d ->
    [Mov (dtToAsmType $ irValToDT lVal) (cvtOperand lVal) (cvtOperand d),
      AsmBinary (irBinaryOpToAsmOp op) (dtToAsmType $ irValToDT lVal)
        (cvtOperand rVal) (cvtOperand d)]
  _ -> undefined
  where cvtOperand = irOperandToAsmOperand m gVarMap

irInstructionToAsmInstruction :: IRInstruction -> M.Map String Int -> M.Map String String
  -> M.Map String IdentifierType -> [AsmInstruction]
irInstructionToAsmInstruction instr m funcList gVarMap = case instr of
  IRReturn val ->
    [Mov (dtToAsmType $ irValToDT val) (cvtOperand val) (Register EAX), Ret]
  IRUnary op s d -> unary
    where unary
            | op == Negate && isFloatDT (irValToDT s) = undefined
            | op == NotRelation =
              [Cmp (dtToAsmType $ irValToDT s) (Imm $ ConstInt 0) (cvtOperand s),
                Mov (dtToAsmType $ irValToDT d) (Imm $ ConstInt 0) (cvtOperand d),
                SetCC E $ cvtOperand d]
            | otherwise =
              [Mov (dtToAsmType $ irValToDT s) (cvtOperand s) (cvtOperand d),
                AsmUnary (irUnaryOpToAsmOp op) (dtToAsmType $ irValToDT s) (cvtOperand d)]
  IRJumpIfZero valToCheck jmpTarget ->
    [Cmp (dtToAsmType $ irValToDT valToCheck) (Imm $ ConstInt 0) (cvtOperand valToCheck), JmpCC E jmpTarget]
  IRJumpIfNotZero valToCheck jmpTarget ->
    [Cmp (dtToAsmType $ irValToDT valToCheck) (Imm $ ConstInt 0) (cvtOperand valToCheck), JmpCC NE jmpTarget]
  IRBinary {} -> irBinaryInstrToAsmInstr instr m gVarMap
  IRJump target -> [AsmJmp target]
  IRLabel target -> [AsmLabel target]
  IRCopy s d -> [Mov (dtToAsmType $ irValToDT s) (cvtOperand s) (cvtOperand d)]
  IRFuncCall name args dst -> irFuncCallToAsm name args dst funcList m gVarMap
  IRSignExtend s d -> [Movsx (cvtOperand s) (cvtOperand d)]
  IRTruncate s d -> [Mov LongWord (cvtOperand s) (cvtOperand d)]
  IRZeroExtend s d -> [MovZeroExtend (cvtOperand s) (cvtOperand d)]
  IRDoubleToInt s d -> undefined
  IRDoubleToUInt s d -> undefined
  IRIntToDouble s d -> undefined
  IRUIntToDouble s d -> undefined
  where cvtOperand = irOperandToAsmOperand m gVarMap

copyParametersToStack :: Int -> (M.Map String Int, [AsmInstruction]) ->
  [(Operand, String)] -> (M.Map String Int, [AsmInstruction])
copyParametersToStack _ (m, instrs) [] = (m, instrs)
copyParametersToStack n (m, instrs) ((src, var) : xs) = 
  copyParametersToStack (n + 1) (M.insert var n m, instrs ++ [Mov QuadWord src (Pseudo (show n))]) xs

buildAsmIntrsForIRRelationOp :: IRInstruction -> M.Map String Int -> M.Map String IdentifierType -> [AsmInstruction]
buildAsmIntrsForIRRelationOp instr m gVarMap =
  let op l r s u
        | isSigned l || isSigned r = s
        | otherwise = u
      (vL, vR, setDst, condCode ) = case instr of
        IRBinary EqualRelation valL valR d -> (valL, valR, d, E)
        IRBinary NotEqualRelation valL valR d -> (valL, valR, d, NE)
        IRBinary GreaterThanRelation valL valR d -> (valL, valR, d, op (irValToDT valL) (irValToDT valR) G A)
        IRBinary GreaterEqualRelation valL valR d -> (valL, valR, d, op (irValToDT valL) (irValToDT valR) GE AE)
        IRBinary LessThanRelation valL valR d -> (valL, valR, d, op (irValToDT valL) (irValToDT valR) L B)
        IRBinary LessEqualRelation valL valR d -> (valL, valR, d, op (irValToDT valL) (irValToDT valR) LE BE)
        _ -> undefined in
  [Cmp (dtToAsmType $ irValToDT vL) (cvtOperand vR) (cvtOperand vL),
    Mov QuadWord (Imm $ ConstInt 0) $ cvtOperand setDst,
    SetCC condCode $ cvtOperand setDst]
  where cvtOperand = irOperandToAsmOperand m gVarMap

noExecutableStackString :: String
noExecutableStackString =
  tabulate [".section", ".note.GNU-stack,\"\",@progbits"] ++ "\n"

convertAsmTempVarToStackAddr :: [AsmInstruction] -> [AsmInstruction]
convertAsmTempVarToStackAddr = map convertInstr
  where convertInstr instr = case instr of
          Mov t s d -> Mov t (convertOperand s) (convertOperand d)
          AsmUnary t op d -> AsmUnary t op $ convertOperand d
          AsmBinary t op r l -> AsmBinary t op (convertOperand r) (convertOperand l)
          AsmIdiv t operand -> AsmIdiv t $ convertOperand operand
          AsmDiv t operand -> AsmDiv t $ convertOperand operand
          Cmp t r l -> Cmp t (convertOperand r) (convertOperand l)
          SetCC code d -> SetCC code (convertOperand d)
          Movsx s d -> Movsx (convertOperand s) (convertOperand d)
          MovZeroExtend s d -> MovZeroExtend (convertOperand s) (convertOperand d)
          _ -> instr
        convertOperand operand = case operand of
          Pseudo ident -> Stack $ (-8) * read ident
          _ -> operand

addAllocateStackToFunc :: [AsmInstruction] -> [AsmInstruction]
addAllocateStackToFunc instrs =
  let stackSize = (-1) * getStackSize instrs in
    if stackSize == 0
      then instrs
      else AllocateStack (16 + div stackSize 16 * 16) : instrs

getStackSize :: [AsmInstruction] -> Int
getStackSize = foldl' getMinSize 0
  where getMinSize x y = min x $ takeMinValFromInstr y
        takeMinValFromInstr instr = case instr of
          Mov _ s d -> min (takeValFromOperand s) (takeValFromOperand d)
          AsmUnary _ _ d -> takeValFromOperand d
          AsmBinary _ _ r l -> min (takeValFromOperand r) (takeValFromOperand l)
          AsmIdiv _ d -> takeValFromOperand d
          AsmDiv _ d -> takeValFromOperand d
          Cmp _ r l -> min (takeValFromOperand r) (takeValFromOperand l)
          SetCC _ d -> takeValFromOperand d
          _ -> 0
        takeValFromOperand operand = case operand of
          Stack i -> i
          _ -> 0

replacePseudoRegAllocateStackFixDoubleStackOperand :: AsmFunctionDefine -> AsmFunctionDefine
replacePseudoRegAllocateStackFixDoubleStackOperand afd =
  afd { instructions = concatMap resolveDoubleStackOperand
                      $ concatMap fixCmpConstant
                      $ concatMap fixBitShiftNonImm
                      $ concatMap fixDivConstant
                      $ addAllocateStackToFunc
                      $ convertAsmTempVarToStackAddr
                      $ instructions afd }

isMemoryAddr :: Operand -> Bool
isMemoryAddr (Stack _) = True
isMemoryAddr (Data _) = True
isMemoryAddr _ = False

isImm :: Operand -> Bool
isImm (Imm _) = True
isImm _ = False

resolveDoubleStackOperand :: AsmInstruction -> [AsmInstruction]
resolveDoubleStackOperand instr = case instr of
  instrs@(Mov t i j) -> if isMemoryAddr i && isMemoryAddr j || t == QuadWord && isMemoryAddr j
    then [Mov t i (Register R10),
        Mov t (Register R10) j]
    else [instrs]
  instrs@(AsmBinary AsmMul t mulVal i) -> if isMemoryAddr i || t == QuadWord && isMemoryAddr i
    then [Mov t mulVal (Register R10),
          Mov t i (Register R11),
          AsmBinary AsmMul t (Register R10) $ Register R11,
          Mov t (Register R11) i]
    else [instrs]
  instrs@(AsmBinary op t i j) -> if isMemoryAddr i && isMemoryAddr j || t == QuadWord && isMemoryAddr j
    then [Mov t i (Register R10),
        AsmBinary op t (Register R10) j]
    else [instrs]
  instrs@(Cmp t i j) -> if isMemoryAddr i && isMemoryAddr j || t == QuadWord
    then [Mov t i (Register R10),
          Mov t j (Register R11),
          Cmp t (Register R10) (Register R11)]
    else [instrs]
  instrs@(Movsx i j) -> if isImm i || (isMemoryAddr i && isMemoryAddr j) || isMemoryAddr j
    then [Mov LongWord i $ Register R10,
          Movsx (Register R10) (Register R11),
          Mov QuadWord (Register R11) j]
    else [instrs]
  MovZeroExtend i j -> if isImm i || (isMemoryAddr i && isMemoryAddr j) || isMemoryAddr j
    then [Mov LongWord i (Register R11), Mov QuadWord (Register R11) j]
    else [Mov LongWord i j]
  _ -> [instr]

fixDivConstant :: AsmInstruction -> [AsmInstruction]
fixDivConstant (AsmIdiv t (Imm i)) =
  [Mov t (Imm i) (Register R10D), AsmIdiv t $ Register R10D]
fixDivConstant (AsmIdiv t (Stack i)) =
  [Mov t (Stack i) (Register R10D), AsmIdiv t $ Register R10D]
fixDivConstant (AsmDiv t (Imm i)) =
  [Mov t (Imm i) (Register R10D), AsmDiv t $ Register R10D]
fixDivConstant (AsmDiv t (Stack i)) =
  [Mov t (Stack i) (Register R10D), AsmDiv t $ Register R10D]
fixDivConstant instr = [instr]

fixBitShiftNonImm :: AsmInstruction -> [AsmInstruction]
fixBitShiftNonImm (AsmBinary AsmShiftL t (Stack i) d) =
  [Mov AsmByte  (Stack i) (Register CL), AsmBinary AsmShiftL t (Register CL) d]
fixBitShiftNonImm (AsmBinary AsmUShiftL t (Stack i) d) =
  [Mov AsmByte  (Stack i) (Register CL), AsmBinary AsmUShiftL t (Register CL) d]
fixBitShiftNonImm (AsmBinary AsmShiftR t (Stack i) d) =
  [Mov AsmByte (Stack i) (Register CL), AsmBinary AsmShiftR t (Register CL) d]
fixBitShiftNonImm (AsmBinary AsmUShiftR t (Stack i) d) =
  [Mov AsmByte (Stack i) (Register CL), AsmBinary AsmUShiftR t (Register CL) d]
fixBitShiftNonImm instr = [instr]

fixCmpConstant :: AsmInstruction -> [AsmInstruction]
fixCmpConstant (Cmp t r (Imm l)) = [Mov t (Imm l) (Register R11D), Cmp t r $ Register R11D]
fixCmpConstant instr = [instr]

regQWordMap :: M.Map Reg Reg
regQWordMap = M.fromList $ zip (enumFromTo AL R12)
  [RAX, RAX, RAX, RAX,
    RBX, RBX, RBX, RBX,
    RCX, RCX, RCX, RCX,
    RDX, RDX, RDX, RDX,
    RSI, RSI, RSI, RSI,
    RDI, RDI, RDI, RDI,
    RBP, RBP, RBP, RBP,
    RSP, RSP, RSP, RSP,
    R8, R8, R8, R8,
    R9, R9, R9, R9,
    R10, R10, R10, R10,
    R11, R11, R11, R11,
    R12, R12, R12, R12]

regDWordMap :: M.Map Reg Reg
regDWordMap = M.fromList $ zip (enumFromTo AL R12)
  [EAX, EAX, EAX, EAX,
    EBX, EBX, EBX, EBX,
    ECX, ECX, ECX, ECX,
    EDX, EDX, EDX, EDX,
    ESI, ESI, ESI, ESI,
    EDI, EDI, EDI, EDI,
    EBP, EBP, EBP, EBP,
    ESP, ESP, ESP, ESP,
    R8D, R8D, R8D, R8D,
    R9D, R9D, R9D, R9D,
    R10D, R10D, R10D, R10D,
    R11D, R11D, R11D, R11D,
    R12D, R12D, R12D, R12D]

regWordMap :: M.Map Reg Reg
regWordMap = M.fromList $ zip (enumFromTo AL R12)
  [AX, AX, AX, AX,
    BX, BX, BX, BX,
    CX, CX, CX, CX,
    DX, DX, DX, DX,
    SI, SI, SI, SI,
    DI, DI, DI, DI,
    BP, BP, BP, BP,
    SP, SP, SP, SP,
    R8W, R8W, R8W, R8W,
    R9W, R9W, R9W, R9W,
    R10W, R10W, R10W, R10W,
    R11W, R11W, R11W, R11W,
    R12W, R12W, R12W, R12W]

regByteMap :: M.Map Reg Reg
regByteMap = M.fromList $ zip (enumFromTo AL R12)
  [AL, AL, AL, AL,
    BL, BL, BL, BL,
    CL, CL, CL, CL,
    DL, DL, DL, DL,
    SIL, SIL, SIL, SIL,
    DIL, DIL, DIL, DIL,
    BPL, BPL, BPL, BPL,
    SPL, SPL, SPL, SPL,
    R8B, R8B, R8B, R8B,
    R9B, R9B, R9B, R9B,
    R10B, R10B, R10B, R10B,
    R11B, R11B, R11B, R11B,
    R12B, R12B, R12B, R12B]

convertToNSizeOperand :: MemorySize -> Operand -> Operand
convertToNSizeOperand s op = case op of
  Register r -> case s of
    BYTE -> Register $ regByteMap M.! r
    WORD -> Register $ regWordMap M.! r
    DWORD -> Register $ regDWordMap M.! r
    QWORD -> Register $ regQWordMap M.! r
  _ -> op

tabulate :: [String] -> String
tabulate = intercalate "\t" . ("" :)

asmFuncReturnStr :: [String]
asmFuncReturnStr = 
  [tabulate ["movq", "%rbp, %rsp"],
  tabulate ["popq", "%rbp"],
  tabulate ["ret"]]

asmTypeToAsmStrSuffix :: AsmType -> String
asmTypeToAsmStrSuffix t = case t of
  AsmByte -> "b"
  AsmWord -> "w"
  LongWord -> "l"
  QuadWord -> "q"
  AsmDouble -> "sd"

asmInstructionToStr :: AsmInstruction -> [String]
asmInstructionToStr instr = case instr of
  Ret -> asmFuncReturnStr
  Mov t s d ->
    pure $ tabulate ["mov" ++ asmTypeToAsmStrSuffix t,
    show (convertToNSizeOperand (asmTypeToMemSize t) s) ++ ", " ++ show (convertToNSizeOperand (asmTypeToMemSize t) d)]
  AsmUnary op t d -> pure $ tabulate [show op ++ asmTypeToAsmStrSuffix t, show (convertToNSizeOperand (asmTypeToMemSize t) d)]
  AsmBinary op t r l ->
    let asmMemSize = if op `elem` [AsmShiftL, AsmShiftR, AsmUShiftL, AsmUShiftR]
        then BYTE
        else asmTypeToMemSize t in
    pure $ tabulate [show op ++ asmTypeToAsmStrSuffix t,
    show (convertToNSizeOperand asmMemSize r) ++ ", " ++ show (convertToNSizeOperand (asmTypeToMemSize t) l)]
  Cdq t -> pure $ tabulate [if t == LongWord then "cdq" else "cqo"]
  AsmIdiv t operand -> pure $ tabulate ["idiv" ++ asmTypeToAsmStrSuffix t, show (convertToNSizeOperand (asmTypeToMemSize t) operand)]
  AsmDiv t operand -> pure $ tabulate ["div" ++ asmTypeToAsmStrSuffix t, show (convertToNSizeOperand (asmTypeToMemSize t) operand)]
  AllocateStack i -> case i of
    0 -> []
    _ -> pure $ tabulate ["subq", "$" ++ show i ++ ", %rsp"]
  Cmp t r l ->
    pure $ tabulate ["cmp" ++ asmTypeToAsmStrSuffix t,
    show (convertToNSizeOperand (asmTypeToMemSize t) r) ++ ", " ++ show (convertToNSizeOperand (asmTypeToMemSize t) l)]
  AsmJmp target -> pure $ tabulate ["jmp", ".L" ++ target]
  JmpCC code target -> pure $ tabulate ["j" ++ show code, ".L" ++ target]
  SetCC code dst -> pure $ tabulate ["set" ++ show code, show dst]
  AsmLabel target ->  [".L" ++  target ++ ":"]
  Call name -> pure $ tabulate ["call", name]
  Push s -> pure $ tabulate ["pushq", show $ convertToNSizeOperand QWORD s]
  DeallocateStack i -> case i of
    0 -> []
    _ -> pure $ tabulate ["addq", "$" ++ show i ++ ", %rsp"]
  Movsx s d -> pure $ tabulate ["movslq", show (convertToNSizeOperand DWORD s) ++ ", " ++ show (convertToNSizeOperand QWORD d)]
  MovZeroExtend _ _ -> undefined

asmFunctionDefineToStr :: AsmFunctionDefine -> String
asmFunctionDefineToStr (AsmFunctionDefine fname isGlobal instrs) =
  unlines [tabulate $ if isGlobal then [".globl", fname] else [],
    tabulate [".text"],
    fname ++ ":",
    tabulate ["pushq", "%rbp"],
    tabulate ["movq", "%rsp, %rbp"],
    unlines $ concatMap asmInstructionToStr instrs]

asmStaticVarDefineToStr :: AsmStaticVarDefine -> String
asmStaticVarDefineToStr (AsmStaticVarDefine vName isGlobal varAlign initVal) =
  unlines [tabulate $ if isGlobal then [".globl", vName] else [],
    tabulate [".data"],
    tabulate [".align " ++ show varAlign],
    vName ++ ":",
    tabulate ["." ++ show (staticInitToAsmType initVal) ++ " " ++ show (staticInitToInt initVal)]]
