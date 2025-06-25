-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Assembly.hs                                        :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:33:35 by mayeung           #+#    #+#             --
--   Updated: 2025/06/25 10:31:50 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Assembly where

import IR
import Operation
import Data.List

type AsmProgramAST = [AsmFunctionDefine]

data AsmFunctionDefine =
  AsmFunctionDefine
  {
    asmFuncName :: String,
    instructions :: [AsmInstruction]
  }
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
  E
  | NE
  | G
  | GE
  | L
  | LE
  deriving Eq

data Reg =
  AX
  | CX
  | CL
  | DX
  | DI
  | SI
  | R8
  | R9
  | R10
  | R10D
  | R11
  | R11D
  | R12
  | R12D
  deriving Eq

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
  show AX = "eax"
  show CL = "cl"
  show DX = "edx"
  show R10 = "r10"
  show R10D = "r10d"
  show R11 = "r11"
  show R11D = "r11d"
  show R12 = "r12"
  show R12D = "r12d"

irASTToAsmAST :: IRProgramAST -> AsmProgramAST
irASTToAsmAST = map irFuncDefineToAsmFuncDefine

irFuncDefineToAsmFuncDefine :: IRFunctionDefine -> AsmFunctionDefine
irFuncDefineToAsmFuncDefine fd =
  AsmFunctionDefine
    (irFuncName fd)
    (concatMap
      irInstructionToAsmInstruction
      (irInstruction fd))

irOperandToAsmOperand :: IRVal -> Operand
irOperandToAsmOperand (IRConstant i) = Imm $ read i
irOperandToAsmOperand (IRVar s) = Pseudo s

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

irInstructionToAsmInstruction :: IRInstruction -> [AsmInstruction]
irInstructionToAsmInstruction (IRReturn val) =
  [Mov (irOperandToAsmOperand val) (Register AX),
    Ret]
irInstructionToAsmInstruction (IRUnary NotRelation s d) =
  [Cmp (Imm 0) (irOperandToAsmOperand s),
    Mov (Imm 0) (irOperandToAsmOperand d),
    SetCC E $ irOperandToAsmOperand d]
irInstructionToAsmInstruction (IRUnary op s d) =
  [Mov (irOperandToAsmOperand s) (irOperandToAsmOperand d),
    AsmUnary (irUnaryOpToAsmOp op) (irOperandToAsmOperand d)]
irInstructionToAsmInstruction (IRBinary Division lVal rVal d) =
  [Mov (irOperandToAsmOperand lVal) (Register AX),
    Cdq,
    AsmIdiv $ irOperandToAsmOperand rVal,
    Mov (Register AX) (irOperandToAsmOperand d)]
irInstructionToAsmInstruction (IRBinary Modulo lVal rVal d) =
  [Mov (irOperandToAsmOperand lVal) (Register AX),
    Cdq,
    AsmIdiv $ irOperandToAsmOperand rVal,
    Mov (Register DX) (irOperandToAsmOperand d)]
irInstructionToAsmInstruction (IRBinary BitShiftLeft lVal rVal d) =
  [Mov (irOperandToAsmOperand lVal) (Register R12D),
    AsmBinary
      (irBinaryOpToAsmOp BitShiftLeft)
      (irOperandToAsmOperand rVal)
      (Register R12D),
    Mov (Register R12D) (irOperandToAsmOperand d)]
irInstructionToAsmInstruction (IRBinary BitShiftRight lVal rVal d) =
  [Mov (irOperandToAsmOperand lVal) (Register R12D),
    AsmBinary
      (irBinaryOpToAsmOp BitShiftRight)
      (irOperandToAsmOperand rVal)
      (Register R12D),
    Mov (Register R12D) (irOperandToAsmOperand d)]
irInstructionToAsmInstruction (IRJumpIfZero valToCheck jmpTarget) =
  [Cmp (Imm 0) (irOperandToAsmOperand valToCheck),
    JmpCC E jmpTarget]
irInstructionToAsmInstruction (IRJumpIfNotZero valToCheck jmpTarget) =
  [Cmp (Imm 0) (irOperandToAsmOperand valToCheck),
    JmpCC NE jmpTarget]
irInstructionToAsmInstruction (IRBinary EqualRelation valL valR d) =
  buildAsmIntrsForIRRelationOp E valL valR d
irInstructionToAsmInstruction (IRBinary NotEqualRelation valL valR d) =
  buildAsmIntrsForIRRelationOp NE valL valR d
irInstructionToAsmInstruction (IRBinary GreaterThanRelation valL valR d) =
  buildAsmIntrsForIRRelationOp G valL valR d
irInstructionToAsmInstruction (IRBinary GreaterEqualRelation valL valR d) =
  buildAsmIntrsForIRRelationOp GE valL valR d
irInstructionToAsmInstruction (IRBinary LessThanRelation valL valR d) =
  buildAsmIntrsForIRRelationOp L valL valR d
irInstructionToAsmInstruction (IRBinary LessEqualRelation valL valR d) =
  buildAsmIntrsForIRRelationOp LE valL valR d
irInstructionToAsmInstruction (IRBinary op lVal rVal d) =
  [Mov (irOperandToAsmOperand lVal) (irOperandToAsmOperand d),
    AsmBinary
      (irBinaryOpToAsmOp op)
      (irOperandToAsmOperand rVal)
      (irOperandToAsmOperand d)]
irInstructionToAsmInstruction (IRJump target) = [AsmJmp target]
irInstructionToAsmInstruction (IRLabel target) = [AsmLabel target]
irInstructionToAsmInstruction (IRCopy s d) = 
  [Mov (irOperandToAsmOperand s) (irOperandToAsmOperand d)]

buildAsmIntrsForIRRelationOp :: CondCode -> IRVal -> IRVal -> IRVal -> [AsmInstruction]
buildAsmIntrsForIRRelationOp condCode valL valR setDst =
  [Cmp (irOperandToAsmOperand valR) (irOperandToAsmOperand valL),
    Mov (Imm 0) (irOperandToAsmOperand setDst),
    SetCC condCode $ irOperandToAsmOperand setDst]

noExecutableStackString :: String
noExecutableStackString =
  tabulate [".section", ".note.GNU-stack,\"\",@progbits"] ++ "\n"

convertAsmTempVarToStackAddr :: [AsmInstruction] -> [AsmInstruction]
convertAsmTempVarToStackAddr = map convertInstr
  where convertInstr instr =
          case instr of
            Mov s d -> Mov (convertOperand s) (convertOperand d)
            Movb s d -> Movb (convertOperand s) (convertOperand d)
            AsmUnary op d -> AsmUnary op $ convertOperand d
            AsmBinary op r l -> AsmBinary op (convertOperand r) (convertOperand l)
            AsmIdiv operand -> AsmIdiv $ convertOperand operand
            Cmp r l -> Cmp (convertOperand r) (convertOperand l)
            SetCC code d -> SetCC code (convertOperand d)
            _ -> instr
        convertOperand operand = 
          case operand of
            Pseudo ident -> Stack $ (-4) * read ident
            _ -> operand

addAllocateStackToFunc :: [AsmInstruction] -> [AsmInstruction]
addAllocateStackToFunc instrs = AllocateStack ((-1) * getStackSize instrs) : instrs

getStackSize :: [AsmInstruction] -> Int
getStackSize = foldl getMinSize 0
  where getMinSize x y = min x $ takeMinValFromInstr y
        takeMinValFromInstr instr =
          case instr of
            Mov s d -> min (takeValFromOperand s) (takeValFromOperand d)
            Movb s d -> min (takeValFromOperand s) (takeValFromOperand d)
            AsmUnary _ d -> takeValFromOperand d
            AsmBinary _ r l -> min (takeValFromOperand r) (takeValFromOperand l)
            AsmIdiv d -> takeValFromOperand d
            Cmp r l -> min (takeValFromOperand r) (takeValFromOperand l)
            SetCC _ d -> takeValFromOperand d
            _ -> 0
        takeValFromOperand operand =
          case operand of
            Stack i -> i
            _ -> 0

replacePseudoRegAllocateStackFixDoubleStackOperand :: AsmFunctionDefine -> AsmFunctionDefine
replacePseudoRegAllocateStackFixDoubleStackOperand afd =
  afd {instructions = concatMap resolveDoubleStackOperand
                    $ concatMap fixCmpConstant
                    $ concatMap fixBitShiftNonImm
                    $ concatMap fixDivConstant
                    $ addAllocateStackToFunc
                    $ convertAsmTempVarToStackAddr
                    $ instructions afd}

resolveDoubleStackOperand :: AsmInstruction -> [AsmInstruction]
resolveDoubleStackOperand instr =
  case instr of
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

asmProgramASTToAsm :: [AsmFunctionDefine] -> String
asmProgramASTToAsm = unlines . map asmFunctionDefineToStr

tabulate :: [String] -> String
tabulate = intercalate "\t" . ("" :)

asmFuncReturnStr :: [String]
asmFuncReturnStr = 
  [tabulate ["movq", "%rbp, %rsp"],
  tabulate ["popq", "%rbp"],
  tabulate ["ret"]]

asmInstructionToStr :: AsmInstruction -> [String]
asmInstructionToStr Ret = asmFuncReturnStr
asmInstructionToStr (Mov s d) = pure $ tabulate ["movl", show s ++ ", " ++ show d]
asmInstructionToStr (Movb s d) = pure $ tabulate ["movb", show s ++ ", " ++ show d]
asmInstructionToStr (AsmUnary op d) = pure $ tabulate [show op, show d]
asmInstructionToStr (AsmBinary op r l) = pure $ tabulate [show op, show r ++ ", " ++ show l]
asmInstructionToStr Cdq = pure $ tabulate ["cdq"]
asmInstructionToStr (AsmIdiv operand) = pure $ tabulate ["idiv", show operand]
asmInstructionToStr (AllocateStack i) = 
  case i of
    0 -> []
    _ -> pure $ tabulate ["subq", "$" ++ show i ++ ", %rsp"]
asmInstructionToStr (Cmp r l) = pure $ tabulate ["cmpl", show r ++ ", " ++ show l]
asmInstructionToStr (AsmJmp target) = pure $ tabulate ["jmp", ".L" ++ target]
asmInstructionToStr (JmpCC code target) = pure $ tabulate ["j" ++ show code, ".L" ++ target]
asmInstructionToStr (SetCC code dst) = pure $ tabulate ["set" ++ show code, show dst]
asmInstructionToStr (AsmLabel target) =  [".L" ++  target ++ ":"]

asmFunctionDefineToStr :: AsmFunctionDefine -> String
asmFunctionDefineToStr (AsmFunctionDefine fname instrs) =
  unlines [tabulate [".globl", fname],
    fname ++ ":",
    tabulate ["pushq", "%rbp"],
    tabulate ["movq", "%rsp, %rbp"],
    unlines $ concatMap asmInstructionToStr instrs]
