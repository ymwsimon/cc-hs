-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Assembly.hs                                        :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:33:35 by mayeung           #+#    #+#             --
--   Updated: 2025/07/08 15:27:10 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Assembly where

import IR
import Operation
import Data.List
import qualified Data.Map.Strict as M
import Parser
import Data.Int

type AsmProgramAST = [AsmTopLevel]

data AsmTopLevel =
  AsmFunc {asmFuncD :: AsmFunctionDefine}
  | AsmStaticVar {asmVarD :: AsmStaticVarDefine}
  deriving (Show, Eq)

data AsmStaticVarDefine =
  AsmStaticVarDefine {asmVarName :: String, asmVarGlobal :: Bool, asmVarAlign :: Int, asmVarInit :: StaticInit}
  deriving (Show, Eq)

data AsmFunctionDefine =
  AsmFunctionDefine {asmFuncName :: String, asmFuncGlobal :: Bool, instructions :: [AsmInstruction]}
  deriving (Show, Eq)

data AsmInstruction =
  Mov {asmMovType :: AsmType , asmMovSrc :: Operand, asmMovDst :: Operand}
  | Movb {asmMovsrcB :: Operand, asmMovDstB :: Operand}
  | Movsx {asmMovsxSrc :: Operand, asmMovsxDst :: Operand}
  | Ret
  | AsmUnary AsmUnaryOp AsmType Operand
  | AsmBinary AsmBinaryOp AsmType Operand Operand
  | AsmIdiv AsmType Operand
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
  LongWord
  | QuadWord
  deriving Eq

data AsmUnaryOp =
  AsmNeg
  | AsmNot
  deriving Eq

data AsmBinaryOp =
  AsmPlus
  | AsmMius
  | AsmMul
  | AsmDiv
  | AsmMod
  | AsmBitAnd
  | AsmBitOr
  | AsmBitXor
  | AsmShiftL
  | AsmShiftR
  deriving Eq

data CondCode =
  E | NE
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
  deriving (Eq, Enum, Ord)

data MemorySize =
  Byte
  | WORD
  | DWORD
  | QWORD
  deriving (Show, Eq)

data Operand =
  Imm Int64
  | Register Reg
  | Pseudo {identifier :: String}
  | Stack Int
  | Data String
  deriving Eq

instance Show AsmType where
  show LongWord = "long"
  show QuadWord = "quad"

instance Show Operand where
  show (Imm i) = "$" ++ show i
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
  show AsmDiv = undefined
  show AsmMod = undefined
  show AsmBitAnd = "and"
  show AsmBitOr = "or"
  show AsmBitXor = "xor"
  show AsmShiftL = "sal"
  show AsmShiftR = "sar"

instance Show CondCode where
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

isAsmFuncDefine :: AsmTopLevel -> Bool
isAsmFuncDefine (AsmFunc _) = True
isAsmFuncDefine _ = False

isAsmStaticVarDefine :: AsmTopLevel -> Bool
isAsmStaticVarDefine (AsmStaticVar _) = True
isAsmStaticVarDefine _ = False

staticInitToAsmType :: StaticInit -> AsmType
staticInitToAsmType si = case si of
  IntInit _ -> LongWord
  _ -> QuadWord

staticInitToAsmSize :: StaticInit -> Int
staticInitToAsmSize si = case si of
  IntInit _ -> 4
  _ -> 8

dtToAsmSize :: DT -> Int
dtToAsmSize dt = case dt of
  DTInternal TInt -> 4
  DTInternal TLong -> 8
  _ -> 0

dtToAsmType :: DT -> AsmType
dtToAsmType dt = case dt of
  DTInternal TInt -> LongWord
  DTInternal TLong -> QuadWord
  _ -> LongWord

asmTypeToMemSize :: AsmType -> MemorySize
asmTypeToMemSize t = case t of
  LongWord -> DWORD
  QuadWord -> QWORD

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

irOperandToAsmOperand :: IRVal -> M.Map String Int -> M.Map String IdentifierType -> Operand
irOperandToAsmOperand (IRConstant _ i) _ _ = Imm $ numConstToInt i
irOperandToAsmOperand (IRVar _ s) m gVarMap
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
irBinaryOpToAsmOp Division = AsmDiv
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
        zipWith (\pr a -> Mov (dtToAsmType $ irValToDT a) (irOperandToAsmOperand a m gVarMap) pr)
          parametersRegister regArg
      copyStackArgsInstr = concatMap (\sa -> case sa of
        IRConstant _ c -> [Push (Imm $ numConstToInt c)]
        IRVar t _ ->
          [Mov (dtToAsmType t)(irOperandToAsmOperand sa m gVarMap) (Register RAX),
            Push (Register RAX)]) (reverse stackArg)
      callInstrs = if M.member name funcList
          then [Call name] else [Call $ name ++ "@PLT"] in
  paddingInstr ++ copyRegArgsInstr ++ copyStackArgsInstr ++ callInstrs ++
    [DeallocateStack (8 * length stackArg + paddingSize)] ++
    [Mov (dtToAsmType $ irValToDT dst) (Register EAX) (irOperandToAsmOperand dst m gVarMap)]

irInstructionToAsmInstruction :: IRInstruction -> M.Map String Int -> M.Map String String
  -> M.Map String IdentifierType -> [AsmInstruction]
irInstructionToAsmInstruction instr m funcList gVarMap = case instr of
  IRReturn val ->
    [Mov (dtToAsmType $ irValToDT val) (irOperandToAsmOperand val m gVarMap) (Register EAX), Ret]
  IRUnary NotRelation s d ->
    [Cmp (dtToAsmType $ irValToDT s) (Imm 0) (irOperandToAsmOperand s m gVarMap),
      Mov (dtToAsmType $ irValToDT d) (Imm 0) (irOperandToAsmOperand d m gVarMap),
      SetCC E $ irOperandToAsmOperand d m gVarMap]
  IRUnary op s d ->
    [Mov (dtToAsmType $ irValToDT s) (irOperandToAsmOperand s m gVarMap) (irOperandToAsmOperand d m gVarMap),
      AsmUnary (irUnaryOpToAsmOp op) (dtToAsmType $ irValToDT s) (irOperandToAsmOperand d m gVarMap)]
  IRBinary Division lVal rVal d ->
    [Mov (dtToAsmType $ irValToDT lVal) (irOperandToAsmOperand lVal m gVarMap) (Register AX),
      Cdq (dtToAsmType $ irValToDT lVal),
      AsmIdiv (dtToAsmType $ irValToDT lVal) $ irOperandToAsmOperand rVal m gVarMap,
      Mov (dtToAsmType $ irValToDT lVal) (Register AX) (irOperandToAsmOperand d m gVarMap)]
  IRBinary Modulo lVal rVal d ->
    [Mov (dtToAsmType $ irValToDT lVal) (irOperandToAsmOperand lVal m gVarMap) (Register AX),
      Cdq (dtToAsmType $ irValToDT lVal),
      AsmIdiv (dtToAsmType $ irValToDT lVal) $ irOperandToAsmOperand rVal m gVarMap,
      Mov (dtToAsmType $ irValToDT lVal) (Register DX) (irOperandToAsmOperand d m gVarMap)]
  IRBinary BitShiftLeft lVal rVal d ->
    [Mov (dtToAsmType $ irValToDT lVal) (irOperandToAsmOperand lVal m gVarMap) (Register R12D),
      AsmBinary (irBinaryOpToAsmOp BitShiftLeft) (dtToAsmType $ irValToDT lVal)
        (irOperandToAsmOperand rVal m gVarMap) (Register R12D),
      Mov (dtToAsmType $ irValToDT lVal) (Register R12D) (irOperandToAsmOperand d m gVarMap)]
  IRBinary BitShiftRight lVal rVal d ->
    [Mov (dtToAsmType $ irValToDT lVal) (irOperandToAsmOperand lVal m gVarMap) (Register R12D),
      AsmBinary (irBinaryOpToAsmOp BitShiftRight) (dtToAsmType $ irValToDT lVal)
        (irOperandToAsmOperand rVal m gVarMap) (Register R12D),
      Mov (dtToAsmType $ irValToDT lVal) (Register R12D) (irOperandToAsmOperand d m gVarMap)]
  IRJumpIfZero valToCheck jmpTarget ->
    [Cmp (dtToAsmType $ irValToDT valToCheck) (Imm 0) (irOperandToAsmOperand valToCheck m gVarMap), JmpCC E jmpTarget]
  IRJumpIfNotZero valToCheck jmpTarget ->
    [Cmp (dtToAsmType $ irValToDT valToCheck) (Imm 0) (irOperandToAsmOperand valToCheck m gVarMap), JmpCC NE jmpTarget]
  IRBinary EqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary NotEqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary GreaterThanRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary GreaterEqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary LessThanRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary LessEqualRelation _ _ _ -> buildAsmIntrsForIRRelationOp instr m gVarMap
  IRBinary op lVal rVal d ->
    [Mov (dtToAsmType $ irValToDT lVal) (irOperandToAsmOperand lVal m gVarMap) (irOperandToAsmOperand d m gVarMap),
      AsmBinary (irBinaryOpToAsmOp op) (dtToAsmType $ irValToDT lVal)
        (irOperandToAsmOperand rVal m gVarMap) (irOperandToAsmOperand d m gVarMap)]
  IRJump target -> [AsmJmp target]
  IRLabel target -> [AsmLabel target]
  IRCopy s d -> [Mov (dtToAsmType $ irValToDT s) (irOperandToAsmOperand s m gVarMap) (irOperandToAsmOperand d m gVarMap)]
  IRFuncCall name args dst -> irFuncCallToAsm name args dst funcList m gVarMap
  IRSignExtend s d -> [Movsx (irOperandToAsmOperand s m gVarMap) (irOperandToAsmOperand d m gVarMap)]
  IRTruncate s d -> [Mov LongWord (irOperandToAsmOperand s m gVarMap) (irOperandToAsmOperand d m gVarMap)]

copyParametersToStack :: Int -> (M.Map String Int, [AsmInstruction]) ->
  [(Operand, String)] -> (M.Map String Int, [AsmInstruction])
copyParametersToStack _ (m, instrs) [] = (m, instrs)
copyParametersToStack n (m, instrs) ((src, var) : xs) = 
  copyParametersToStack (n + 1) (M.insert var n m, instrs ++ [Mov QuadWord src (Pseudo (show n))]) xs

buildAsmIntrsForIRRelationOp :: IRInstruction -> M.Map String Int -> M.Map String IdentifierType -> [AsmInstruction]
buildAsmIntrsForIRRelationOp instr m gVarMap =
  let (vL, vR, setDst, condCode ) = case instr of
        IRBinary EqualRelation valL valR d -> (valL, valR, d, E)
        IRBinary NotEqualRelation valL valR d -> (valL, valR, d, NE)
        IRBinary GreaterThanRelation valL valR d -> (valL, valR, d, G)
        IRBinary GreaterEqualRelation valL valR d -> (valL, valR, d, GE)
        IRBinary LessThanRelation valL valR d -> (valL, valR, d, L)
        IRBinary LessEqualRelation valL valR d -> (valL, valR, d, LE)
        _ -> undefined in
  [Cmp (dtToAsmType $ irValToDT vL) (irOperandToAsmOperand vR m gVarMap) (irOperandToAsmOperand vL m gVarMap),
    Mov LongWord (Imm 0) (irOperandToAsmOperand setDst m gVarMap),
    SetCC condCode (irOperandToAsmOperand setDst m gVarMap)]

noExecutableStackString :: String
noExecutableStackString =
  tabulate [".section", ".note.GNU-stack,\"\",@progbits"] ++ "\n"

convertAsmTempVarToStackAddr :: [AsmInstruction] -> [AsmInstruction]
convertAsmTempVarToStackAddr = map convertInstr
  where convertInstr instr = case instr of
          Mov t s d -> Mov t (convertOperand s) (convertOperand d)
          Movb s d -> Movb (convertOperand s) (convertOperand d)
          AsmUnary t op d -> AsmUnary t op $ convertOperand d
          AsmBinary t op r l -> AsmBinary t op (convertOperand r) (convertOperand l)
          AsmIdiv t operand -> AsmIdiv t $ convertOperand operand
          Cmp t r l -> Cmp t (convertOperand r) (convertOperand l)
          SetCC code d -> SetCC code (convertOperand d)
          Movsx s d -> Movsx (convertOperand s) (convertOperand d)
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
          Movb s d -> min (takeValFromOperand s) (takeValFromOperand d)
          AsmUnary _ _ d -> takeValFromOperand d
          AsmBinary _ _ r l -> min (takeValFromOperand r) (takeValFromOperand l)
          AsmIdiv _ d -> takeValFromOperand d
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
  instrs@(Mov t i j) -> if isMemoryAddr i && isMemoryAddr j
    then [Mov t i (Register R10D),
        Mov t (Register R10D) j]
    else [instrs]
  instrs@(AsmBinary AsmMul t mulVal i) -> if isMemoryAddr i
    then [Mov t i (Register R11D),
      AsmBinary AsmMul t mulVal $ Register R11D,
      Mov t (Register R11D) i]
    else [instrs]
  instrs@(AsmBinary op t i j) -> if isMemoryAddr i && isMemoryAddr j
    then [Mov t i (Register R10D),
        AsmBinary op t (Register R10D) j]
    else [instrs]
  instrs@(Cmp t i j) -> if isMemoryAddr i && isMemoryAddr j
    then [Mov t j (Register R10D),
      Cmp t i (Register R10D)]
    else [instrs]
  instrs@(Movsx i j) -> if isImm i || (isMemoryAddr i && isMemoryAddr j) || isMemoryAddr j
    then [Mov LongWord i $ Register R10D,
          Movsx (Register R10D) (Register R11),
          Mov QuadWord (Register R11) j]
    else [instrs]
  _ -> [instr]

fixDivConstant :: AsmInstruction -> [AsmInstruction]
fixDivConstant (AsmIdiv t (Imm i)) =
  [Mov t (Imm i) (Register R10D), AsmIdiv t $ Register R10D]
fixDivConstant (AsmIdiv t (Stack i)) =
  [Mov t (Stack i) (Register R10D), AsmIdiv t $ Register R10D]
fixDivConstant instr = [instr]

fixBitShiftNonImm :: AsmInstruction -> [AsmInstruction]
fixBitShiftNonImm (AsmBinary AsmShiftL t (Stack i) d) =
  [Movb  (Stack i) (Register CL), AsmBinary AsmShiftL t (Register CL) d]
fixBitShiftNonImm (AsmBinary AsmShiftR t (Stack i) d) =
  [Movb (Stack i) (Register CL), AsmBinary AsmShiftR t (Register CL) d]
fixBitShiftNonImm instr = [instr]

fixCmpConstant :: AsmInstruction -> [AsmInstruction]
fixCmpConstant (Cmp t r (Imm l)) = [Mov t (Imm l) (Register R11D), Cmp t r $ Register R11D]
fixCmpConstant instr = [instr]

regQWordMap :: M.Map Reg Reg
regQWordMap = M.fromList $ zip (enumFrom AL)
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
regDWordMap = M.fromList $ zip (enumFrom AL)
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
regWordMap = M.fromList $ zip (enumFrom AL)
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
regByteMap = M.fromList $ zip (enumFrom AL)
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
    Byte -> Register $ regByteMap M.! r
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
  LongWord -> "l"
  QuadWord -> "q"

asmInstructionToStr :: AsmInstruction -> [String]
asmInstructionToStr instr = case instr of
  Ret -> asmFuncReturnStr
  Mov t s d ->
    pure $ tabulate ["mov" ++ asmTypeToAsmStrSuffix t,
    show (convertToNSizeOperand (asmTypeToMemSize t) s) ++ ", " ++ show (convertToNSizeOperand (asmTypeToMemSize t) d)]
  Movb s d -> 
    pure $ tabulate ["movb",
    show (convertToNSizeOperand Byte s) ++ ", " ++ show (convertToNSizeOperand Byte d)]
  AsmUnary op t d -> pure $ tabulate [show op ++ asmTypeToAsmStrSuffix t, show (convertToNSizeOperand (asmTypeToMemSize t) d)]
  AsmBinary AsmShiftL t r l ->
    pure $ tabulate [show AsmShiftL ++ asmTypeToAsmStrSuffix t, show (convertToNSizeOperand Byte r) ++ ", " ++ show l]
  AsmBinary AsmShiftR t r l ->
    pure $ tabulate [show AsmShiftR ++ asmTypeToAsmStrSuffix t, show (convertToNSizeOperand Byte r) ++ ", " ++ show l]
  AsmBinary op t r l ->
    pure $ tabulate [show op ++ asmTypeToAsmStrSuffix t,
    show (convertToNSizeOperand (asmTypeToMemSize t) r) ++ ", " ++ show (convertToNSizeOperand (asmTypeToMemSize t) l)]
  Cdq t -> pure $ tabulate [if t == LongWord then "cdq" else "cqo"]
  AsmIdiv t operand -> pure $ tabulate ["idiv" ++ asmTypeToAsmStrSuffix t, show (convertToNSizeOperand (asmTypeToMemSize t) operand)]
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
