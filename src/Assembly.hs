-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Assembly.hs                                        :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:33:35 by mayeung           #+#    #+#             --
--   Updated: 2025/04/04 17:17:16 by mayeung          ###   ########.fr       --
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
  Mov {src :: Operand, dst :: Operand}
  | Movb {srcB :: Operand, dstB :: Operand}
  | Ret
  | AsmUnary AsmUnaryOp Operand
  | AsmBinary AsmBinaryOp Operand Operand
  | AsmIdiv Operand
  | Cdq
  | AllocateStack Int
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

data Reg =
  AX
  | CL
  | DX
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
  show AsmDiv = "error???"
  show AsmMod = "error???"
  show AsmBitAnd = "andl"
  show AsmBitOr = "orl"
  show AsmBitXor = "xorl"
  show AsmShiftL = "sall"
  show AsmShiftR = "sarl"

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
irInstructionToAsmInstruction (IRUnary op s d) =
  [Mov (irOperandToAsmOperand s) (irOperandToAsmOperand d),
    AsmUnary (irUnaryOpToAsmOp op) (irOperandToAsmOperand d)]
irInstructionToAsmInstruction (IRBinary Division lVal rVal bDst) =
  [Mov (irOperandToAsmOperand lVal) (Register AX),
    Cdq,
    AsmIdiv (irOperandToAsmOperand rVal),
    Mov (Register AX) (irOperandToAsmOperand bDst)]
irInstructionToAsmInstruction (IRBinary Modulo lVal rVal bDst) =
  [Mov (irOperandToAsmOperand lVal) (Register AX),
    Cdq,
    AsmIdiv (irOperandToAsmOperand rVal),
    Mov (Register DX) (irOperandToAsmOperand bDst)]
irInstructionToAsmInstruction (IRBinary BitShiftLeft lVal rVal bDst) =
  [Mov (irOperandToAsmOperand lVal) (Register R12D),
    AsmBinary
      (irBinaryOpToAsmOp BitShiftLeft)
      (irOperandToAsmOperand rVal)
      (Register R12D),
    Mov (Register R12D) (irOperandToAsmOperand bDst)]
irInstructionToAsmInstruction (IRBinary BitShiftRight lVal rVal bDst) =
  [Mov (irOperandToAsmOperand lVal) (Register R12D),
    AsmBinary
      (irBinaryOpToAsmOp BitShiftRight)
      (irOperandToAsmOperand rVal)
      (Register R12D),
    Mov (Register R12D) (irOperandToAsmOperand bDst)]
irInstructionToAsmInstruction (IRBinary op lVal rVal bDst) =
  [Mov (irOperandToAsmOperand lVal) (irOperandToAsmOperand bDst),
    AsmBinary
      (irBinaryOpToAsmOp op)
      (irOperandToAsmOperand rVal)
      (irOperandToAsmOperand bDst)]

noExecutableStackString :: String
noExecutableStackString =
  tabulate [".section", ".note.GNU-stack,\"\",@progbits"] ++ "\n"

convertAsmTempVarToStackAddr :: [AsmInstruction] -> [AsmInstruction]
convertAsmTempVarToStackAddr = map convertInstr
  where convertInstr instr =
          case instr of
            Mov s d -> Mov (convertOperand s) (convertOperand d)
            AsmUnary op d -> AsmUnary op $ convertOperand d
            AsmBinary op l r -> AsmBinary op (convertOperand l) (convertOperand r)
            AsmIdiv operand -> AsmIdiv $ convertOperand operand
            _ -> instr
        convertOperand operand = 
          case operand of
            Pseudo ident -> Stack $ (-4) * read ident
            _ -> operand

addAllocateStackToFunc :: [AsmInstruction] -> [AsmInstruction]
addAllocateStackToFunc instrs = AllocateStack ((-1) * getStackSize instrs) : instrs

getStackSize :: [AsmInstruction] -> Int
getStackSize = foldl getMinSize 0
  where getMinSize x y = min x $ takeMaxValFromInstr y
        takeMaxValFromInstr instr =
          case instr of
            Mov s d -> max (takeValFromOperand s) (takeValFromOperand d)
            AsmUnary _ d -> takeValFromOperand d
            _ -> 0
        takeValFromOperand operand =
          case operand of
            Stack i -> i
            _ -> 0

replacePseudoRegAllocateStackFixDoubleStackOperand :: AsmFunctionDefine -> AsmFunctionDefine
replacePseudoRegAllocateStackFixDoubleStackOperand afd =
  afd {instructions = concatMap resolveDoubleStackOperand
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
    AsmBinary AsmMul val (Stack i) ->
      [Mov (Stack i) (Register R11D),
        AsmBinary AsmMul val (Register R11D),
        Mov (Register R11D) (Stack i) ]
    AsmBinary op (Stack i) (Stack j) ->
      [Mov (Stack i) (Register R10D),
        AsmBinary op (Register R10D) (Stack j)]
    _ -> [instr]

fixDivConstant :: AsmInstruction -> [AsmInstruction]
fixDivConstant (AsmIdiv (Imm i)) =
  [Mov (Imm i) (Register R10D), AsmIdiv (Register R10D)]
fixDivConstant (AsmIdiv (Stack i)) =
  [Mov (Stack i) (Register R10D), AsmIdiv (Register R10D)]
fixDivConstant instr = [instr]

fixBitShiftNonImm :: AsmInstruction -> [AsmInstruction]
fixBitShiftNonImm (AsmBinary AsmShiftL (Stack i) d) =
  [Movb (Stack i) (Register CL), AsmBinary AsmShiftL (Register CL) d]
fixBitShiftNonImm (AsmBinary AsmShiftR (Stack i) d) =
  [Movb (Stack i) (Register CL), AsmBinary AsmShiftR (Register CL) d]
fixBitShiftNonImm instr = [instr]

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
asmInstructionToStr (AsmBinary op s d) = pure $ tabulate [show op, show s ++ ", " ++ show d]
asmInstructionToStr Cdq = pure $ tabulate ["cdq"]
asmInstructionToStr (AsmIdiv operand) = pure $ tabulate ["idiv", show operand]
asmInstructionToStr (AllocateStack i) = 
  case i of
    0 -> []
    _ -> pure $ tabulate ["subq", "$" ++ show i ++ ", %rsp"]

asmFunctionDefineToStr :: AsmFunctionDefine -> String
asmFunctionDefineToStr (AsmFunctionDefine fname instrs) =
  unlines [tabulate [".globl", fname],
    fname ++ ":",
    tabulate ["pushq", "%rbp"],
    tabulate ["movq", "%rsp, %rbp"],
    unlines $ concatMap asmInstructionToStr instrs]
