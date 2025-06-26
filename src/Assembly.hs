-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Assembly.hs                                        :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:33:35 by mayeung           #+#    #+#             --
--   Updated: 2025/06/26 22:43:03 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Assembly where

import IR
import Operation
import Data.List
import qualified Data.Map.Strict as M

type AsmProgramAST = [AsmFunctionDefine]

data AsmFunctionDefine =
  AsmFunctionDefine { asmFuncName :: String, instructions :: [AsmInstruction] }
  deriving (Show, Eq)

data AsmInstruction =
  Mov {asmMovSrc :: Operand, asmMovDst :: Operand}
  | Movb {asmMovsrcB :: Operand, asmMovDstB :: Operand}
  | Ret
  | AsmUnary AsmUnaryOp Operand
  | AsmBinary AsmBinaryOp Operand Operand
  | AsmIdiv Operand
  | Cdq
  | AllocateStack Int
  | Cmp Operand Operand
  | AsmJmp {jumpIdentifier :: String}
  | JmpCC {cond :: CondCode, jumpCCIdentifier :: String}
  | SetCC {cond :: CondCode, setCCDst :: Operand}
  | AsmLabel {labelIdentifier :: String}
  | DeallocateStack Int
  | Push Operand
  | Call String
  deriving (Show, Eq)

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
  Imm Int
  | Register Reg
  | Pseudo {identifier :: String}
  | Stack Int
  deriving Eq

instance Show Operand where
  show (Imm i) = "$" ++ show i
  show (Register s) = "%" ++ show s
  show (Pseudo ident) = "tmpVar." ++ show ident
  show (Stack i) = show i ++ "(%rbp)"

instance Show AsmUnaryOp where
  show AsmNeg = "negl"
  show AsmNot = "notl"

instance Show AsmBinaryOp where
  show AsmPlus = "addl"
  show AsmMius = "subl"
  show AsmMul = "imull"
  show AsmDiv = undefined
  show AsmMod = undefined
  show AsmBitAnd = "andl"
  show AsmBitOr = "orl"
  show AsmBitXor = "xorl"
  show AsmShiftL = "sall"
  show AsmShiftR = "sarl"

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

irASTToAsmAST :: IRProgramAST -> AsmProgramAST
irASTToAsmAST irAst = map (irFuncDefineToAsmFuncDefine (getFuncList irAst)) irAst

parametersRegister :: [Operand]
parametersRegister = map Register [EDI, ESI, EDX, ECX, R8D, R9D] ++ map Stack [16, 24..]

getPaddingSize :: [a] -> Int
getPaddingSize args = if odd $ length args then 8 else 0

getFuncList :: IRProgramAST -> M.Map String String
getFuncList = foldl' (\m fd -> M.insert (irFuncName fd) (irFuncName fd) m) M.empty

irFuncDefineToAsmFuncDefine :: M.Map String String -> IRFunctionDefine -> AsmFunctionDefine
irFuncDefineToAsmFuncDefine funcList fd =
  let (m, instrs) = copyParametersToStack
          ((+ 1) . getMaxStackVarId $ irInstruction fd)
          (M.empty, []) (zip parametersRegister (irParameter fd)) in
        AsmFunctionDefine (irFuncName fd) $
          instrs ++
          [AllocateStack (getPaddingSize (irParameter fd))] ++
          concatMap (\irs -> irInstructionToAsmInstruction irs m funcList) (irInstruction fd)

irOperandToAsmOperand :: IRVal -> M.Map String Int -> Operand
irOperandToAsmOperand (IRConstant i) _ = Imm $ read i
irOperandToAsmOperand (IRVar s) m = if M.member s m then Pseudo (show $ m M.! s) else Pseudo s

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

irFuncCallToAsm :: String -> [IRVal] -> IRVal -> M.Map String String -> M.Map String Int -> [AsmInstruction]
irFuncCallToAsm name args dst funcList m =
  let (regArg, stackArg) = splitAt 6 args
      paddingSize = getPaddingSize stackArg
      paddingInstr = [AllocateStack paddingSize]
      copyRegArgsInstr = zipWith (\pr a -> Mov (irOperandToAsmOperand a m) pr) parametersRegister regArg
      copyStackArgsInstr = concatMap (\sa -> case sa of
        IRConstant c -> [Push (Imm $ read c)]
        IRVar _ -> [Mov (irOperandToAsmOperand sa m) (Register RAX), Push (Register RAX)]) (reverse stackArg)
      callInstrs = if M.member name funcList then [Call name] else [Call $ name ++ "@PLT"] in
  paddingInstr ++ copyRegArgsInstr ++ copyStackArgsInstr ++ callInstrs ++
    [DeallocateStack (8 * length stackArg + paddingSize)] ++ [Mov (Register EAX) (irOperandToAsmOperand dst m)]

irInstructionToAsmInstruction :: IRInstruction -> M.Map String Int -> M.Map String String -> [AsmInstruction]
irInstructionToAsmInstruction (IRReturn val) m _ =
  [Mov (irOperandToAsmOperand val m) (Register EAX),
    Ret]
irInstructionToAsmInstruction (IRUnary NotRelation s d) m _ =
  [Cmp (Imm 0) (irOperandToAsmOperand s m),
    Mov (Imm 0) (irOperandToAsmOperand d m),
    SetCC E $ irOperandToAsmOperand d m]
irInstructionToAsmInstruction (IRUnary op s d) m _ =
  [Mov (irOperandToAsmOperand s m) (irOperandToAsmOperand d m),
    AsmUnary (irUnaryOpToAsmOp op) (irOperandToAsmOperand d m)]
irInstructionToAsmInstruction (IRBinary Division lVal rVal d) m _ =
  [Mov (irOperandToAsmOperand lVal m) (Register AX),
    Cdq,
    AsmIdiv $ irOperandToAsmOperand rVal m,
    Mov (Register AX) (irOperandToAsmOperand d m)]
irInstructionToAsmInstruction (IRBinary Modulo lVal rVal d) m _ =
  [Mov (irOperandToAsmOperand lVal m) (Register AX),
    Cdq,
    AsmIdiv $ irOperandToAsmOperand rVal m,
    Mov (Register DX) (irOperandToAsmOperand d m)]
irInstructionToAsmInstruction (IRBinary BitShiftLeft lVal rVal d) m _ =
  [Mov (irOperandToAsmOperand lVal m) (Register R12D),
    AsmBinary
      (irBinaryOpToAsmOp BitShiftLeft)
      (irOperandToAsmOperand rVal m)
      (Register R12D),
    Mov (Register R12D) (irOperandToAsmOperand d m)]
irInstructionToAsmInstruction (IRBinary BitShiftRight lVal rVal d) m _ =
  [Mov (irOperandToAsmOperand lVal m) (Register R12D),
    AsmBinary
      (irBinaryOpToAsmOp BitShiftRight)
      (irOperandToAsmOperand rVal m)
      (Register R12D),
    Mov (Register R12D) (irOperandToAsmOperand d m)]
irInstructionToAsmInstruction (IRJumpIfZero valToCheck jmpTarget) m _ =
  [Cmp (Imm 0) (irOperandToAsmOperand valToCheck m),
    JmpCC E jmpTarget]
irInstructionToAsmInstruction (IRJumpIfNotZero valToCheck jmpTarget) m _ =
  [Cmp (Imm 0) (irOperandToAsmOperand valToCheck m),
    JmpCC NE jmpTarget]
irInstructionToAsmInstruction (IRBinary EqualRelation valL valR d) m _ =
  buildAsmIntrsForIRRelationOp E (irOperandToAsmOperand valL m)
    (irOperandToAsmOperand valR m) (irOperandToAsmOperand d m)
irInstructionToAsmInstruction (IRBinary NotEqualRelation valL valR d) m _ =
  buildAsmIntrsForIRRelationOp NE (irOperandToAsmOperand valL m)
    (irOperandToAsmOperand valR m) (irOperandToAsmOperand d m)
irInstructionToAsmInstruction (IRBinary GreaterThanRelation valL valR d) m _ =
  buildAsmIntrsForIRRelationOp G (irOperandToAsmOperand valL m)
    (irOperandToAsmOperand valR m) (irOperandToAsmOperand d m)
irInstructionToAsmInstruction (IRBinary GreaterEqualRelation valL valR d) m _ =
  buildAsmIntrsForIRRelationOp GE (irOperandToAsmOperand valL m)
    (irOperandToAsmOperand valR m) (irOperandToAsmOperand d m)
irInstructionToAsmInstruction (IRBinary LessThanRelation valL valR d) m _ =
  buildAsmIntrsForIRRelationOp L (irOperandToAsmOperand valL m)
    (irOperandToAsmOperand valR m) (irOperandToAsmOperand d m)
irInstructionToAsmInstruction (IRBinary LessEqualRelation valL valR d) m _ =
  buildAsmIntrsForIRRelationOp LE (irOperandToAsmOperand valL m)
    (irOperandToAsmOperand valR m) (irOperandToAsmOperand d m)
irInstructionToAsmInstruction (IRBinary op lVal rVal d) m _ =
  [Mov (irOperandToAsmOperand lVal m) (irOperandToAsmOperand d m),
    AsmBinary (irBinaryOpToAsmOp op) (irOperandToAsmOperand rVal m)
      (irOperandToAsmOperand d m)]
irInstructionToAsmInstruction (IRJump target) _ _ = [AsmJmp target]
irInstructionToAsmInstruction (IRLabel target) _ _ = [AsmLabel target]
irInstructionToAsmInstruction (IRCopy s d) m _ = 
  [Mov (irOperandToAsmOperand s m) (irOperandToAsmOperand d m)]
irInstructionToAsmInstruction (IRFuncCall name args dst) m funcList = irFuncCallToAsm name args dst funcList m

copyParametersToStack :: Int -> (M.Map String Int, [AsmInstruction])
  -> [(Operand, String)] -> (M.Map String Int, [AsmInstruction])
copyParametersToStack _ (m, instrs) [] = (m, instrs)
copyParametersToStack n (m, instrs) ((src, var) : xs) = 
  copyParametersToStack (n + 1) (M.insert var n m, instrs ++ [Mov src (Pseudo (show n))]) xs

buildAsmIntrsForIRRelationOp :: CondCode -> Operand -> Operand -> Operand -> [AsmInstruction]
buildAsmIntrsForIRRelationOp condCode valL valR setDst =
  [Cmp valR valL,
    Mov (Imm 0) setDst,
    SetCC condCode setDst]

noExecutableStackString :: String
noExecutableStackString =
  tabulate [".section", ".note.GNU-stack,\"\",@progbits"] ++ "\n"

convertAsmTempVarToStackAddr :: [AsmInstruction] -> [AsmInstruction]
convertAsmTempVarToStackAddr = map convertInstr
  where convertInstr instr = case instr of
          Mov s d -> Mov (convertOperand s) (convertOperand d)
          Movb s d -> Movb (convertOperand s) (convertOperand d)
          AsmUnary op d -> AsmUnary op $ convertOperand d
          AsmBinary op r l -> AsmBinary op (convertOperand r) (convertOperand l)
          AsmIdiv operand -> AsmIdiv $ convertOperand operand
          Cmp r l -> Cmp (convertOperand r) (convertOperand l)
          SetCC code d -> SetCC code (convertOperand d)
          _ -> instr
        convertOperand operand = case operand of
          Pseudo ident -> Stack $ (-4) * read ident
          _ -> operand

addAllocateStackToFunc :: [AsmInstruction] -> [AsmInstruction]
addAllocateStackToFunc instrs = AllocateStack ((-1) * getStackSize instrs) : instrs

getStackSize :: [AsmInstruction] -> Int
getStackSize = foldl' getMinSize 0
  where getMinSize x y = min x $ takeMinValFromInstr y
        takeMinValFromInstr instr = case instr of
          Mov s d -> min (takeValFromOperand s) (takeValFromOperand d)
          Movb s d -> min (takeValFromOperand s) (takeValFromOperand d)
          AsmUnary _ d -> takeValFromOperand d
          AsmBinary _ r l -> min (takeValFromOperand r) (takeValFromOperand l)
          AsmIdiv d -> takeValFromOperand d
          Cmp r l -> min (takeValFromOperand r) (takeValFromOperand l)
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

resolveDoubleStackOperand :: AsmInstruction -> [AsmInstruction]
resolveDoubleStackOperand instr = case instr of
  Mov (Stack i) (Stack j) ->
    [Mov (Stack i) (Register R10D),
      Mov (Register R10D) (Stack j)]
  AsmBinary AsmMul mulVal (Stack i) ->
    [Mov (Stack i) (Register R11D),
      AsmBinary AsmMul mulVal $ Register R11D,
      Mov (Register R11D) (Stack i) ]
  AsmBinary op (Stack i) (Stack j) ->
    [Mov (Stack i) (Register R10D),
      AsmBinary op (Register R10D) (Stack j)]
  Cmp (Stack i) (Stack j) ->
    [Mov (Stack j) (Register R10D),
      Cmp (Stack i) (Register R10D)]
  _ -> [instr]

fixDivConstant :: AsmInstruction -> [AsmInstruction]
fixDivConstant (AsmIdiv (Imm i)) =
  [Mov (Imm i) (Register R10D), AsmIdiv $ Register R10D]
fixDivConstant (AsmIdiv (Stack i)) =
  [Mov (Stack i) (Register R10D), AsmIdiv $ Register R10D]
fixDivConstant instr = [instr]

fixBitShiftNonImm :: AsmInstruction -> [AsmInstruction]
fixBitShiftNonImm (AsmBinary AsmShiftL (Stack i) d) =
  [Movb (Stack i) (Register CL), AsmBinary AsmShiftL (Register CL) d]
fixBitShiftNonImm (AsmBinary AsmShiftR (Stack i) d) =
  [Movb (Stack i) (Register CL), AsmBinary AsmShiftR (Register CL) d]
fixBitShiftNonImm instr = [instr]

fixCmpConstant :: AsmInstruction -> [AsmInstruction]
fixCmpConstant (Cmp r (Imm l)) = [Mov (Imm l) (Register R11D), Cmp r $ Register R11D]
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

asmInstructionToStr :: AsmInstruction -> [String]
asmInstructionToStr Ret = asmFuncReturnStr
asmInstructionToStr (Mov s d) = pure $ tabulate ["movl", show (convertToNSizeOperand DWORD s) ++ ", " ++ show (convertToNSizeOperand DWORD d)]
asmInstructionToStr (Movb s d) = pure $ tabulate ["movb", show (convertToNSizeOperand Byte s) ++ ", " ++ show (convertToNSizeOperand Byte d)]
asmInstructionToStr (AsmUnary op d) = pure $ tabulate [show op, show (convertToNSizeOperand DWORD d)]
asmInstructionToStr (AsmBinary AsmShiftL r l) = pure $ tabulate [show AsmShiftL, show (convertToNSizeOperand Byte r) ++ ", " ++ show l]
asmInstructionToStr (AsmBinary AsmShiftR r l) = pure $ tabulate [show AsmShiftR, show (convertToNSizeOperand Byte r) ++ ", " ++ show l]
asmInstructionToStr (AsmBinary op r l) = pure $ tabulate [show op, show (convertToNSizeOperand DWORD r) ++ ", " ++ show (convertToNSizeOperand DWORD l)]
asmInstructionToStr Cdq = pure $ tabulate ["cdq"]
asmInstructionToStr (AsmIdiv operand) = pure $ tabulate ["idiv", show (convertToNSizeOperand DWORD operand)]
asmInstructionToStr (AllocateStack i) = case i of
  0 -> []
  _ -> pure $ tabulate ["subq", "$" ++ show ((16 :: Int) * ceiling (fromIntegral i / (16 :: Double))) ++ ", %rsp"]
asmInstructionToStr (Cmp r l) = pure $ tabulate ["cmpl", show (convertToNSizeOperand DWORD r) ++ ", " ++ show (convertToNSizeOperand DWORD l)]
asmInstructionToStr (AsmJmp target) = pure $ tabulate ["jmp", ".L" ++ target]
asmInstructionToStr (JmpCC code target) = pure $ tabulate ["j" ++ show code, ".L" ++ target]
asmInstructionToStr (SetCC code dst) = pure $ tabulate ["set" ++ show code, show dst]
asmInstructionToStr (AsmLabel target) =  [".L" ++  target ++ ":"]
asmInstructionToStr (Call name) = pure $ tabulate ["call", name]
asmInstructionToStr (Push s) = pure $ tabulate ["pushq", show $ convertToNSizeOperand QWORD s]
asmInstructionToStr (DeallocateStack i) = case i of
  0 -> []
  _ -> pure $ tabulate ["addq", "$" ++ show i ++ ", %rsp"]

asmFunctionDefineToStr :: AsmFunctionDefine -> String
asmFunctionDefineToStr (AsmFunctionDefine fname instrs) =
  unlines [tabulate [".globl", fname],
    fname ++ ":",
    tabulate ["pushq", "%rbp"],
    tabulate ["movq", "%rsp, %rbp"],
    unlines $ concatMap asmInstructionToStr instrs]
