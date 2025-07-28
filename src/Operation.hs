-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Operation.hs                                       :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:50:42 by mayeung           #+#    #+#             --
--   Updated: 2025/07/28 13:54:19 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Operation where

import Data.Bits

data UnaryOp =
  Complement
  | Negate
  | UPlus
  | NotRelation
  | PreIncrement
  | PreDecrement
  | PostIncrement
  | PostDecrement
  deriving (Show, Eq)

data BinaryOp =
  Plus
  | Minus
  | Multiply
  | Division
  | Modulo
  | BitOr
  | BitAnd
  | BitXor
  | BitShiftLeft
  | BitShiftRight
  | LogicAnd
  | LogicOr
  | EqualRelation
  | NotEqualRelation
  | LessThanRelation
  | LessEqualRelation
  | GreaterThanRelation
  | GreaterEqualRelation
  | None
  deriving (Show, Eq)

data CompoundAssignOp =
  PlusAssign
  | MinusAssign
  | MultiplyAssign
  | DivisionAssign
  | ModuloAssign
  | BitOrAssign
  | BitAndAssign
  | BitXorAssign
  | BitShiftLeftAssign
  | BitShiftRightAssign
  | AssignOp
  deriving (Show, Eq, Ord)

unaryOpToHaskellOperator :: (Num a, Bits a) => UnaryOp -> a -> a
unaryOpToHaskellOperator op = case op of
  Complement -> complement
  Negate -> negate
  UPlus -> id
  NotRelation -> (\u -> if popCount u == 0 then 1 else 0)
  _ -> undefined

unaryOpToHaskellOperatorDouble :: (Eq a, Fractional a) => UnaryOp -> a -> a
unaryOpToHaskellOperatorDouble op = case op of
  Negate -> negate
  UPlus -> id
  NotRelation -> (\u -> if u == 0.0 then 1.0 else 0.0)
  _ -> undefined

binaryOpToHaskellOperator :: (Integral a, Bits a) => BinaryOp -> a -> a -> a
binaryOpToHaskellOperator op = case op of
  Plus -> (+)
  Minus -> (-)
  Multiply -> (*)
  Division -> quot
  Modulo -> rem
  BitOr -> (.|.)
  BitAnd -> (.&.)
  BitXor -> xor
  BitShiftLeft -> (\i n -> shiftL i (fromIntegral n))
  BitShiftRight -> (\i n -> shiftR i (fromIntegral n))
  LogicAnd -> (\f s -> if f == 0 || s == 0 then 0 else 1)
  LogicOr -> (\f s -> if f /= 0 || s /= 0 then 1 else 0)
  EqualRelation -> (\l r -> if l == r then 1 else 0)
  NotEqualRelation -> (\l r -> if l /= r then 1 else 0)
  LessThanRelation -> (\l r -> if l < r then 1 else 0)
  LessEqualRelation -> (\l r -> if l <= r then 1 else 0)
  GreaterThanRelation -> (\l r -> if l > r then 1 else 0)
  GreaterEqualRelation -> (\l r -> if l >= r then 1 else 0)
  _ -> undefined

binaryOpToHaskellOperatorDouble :: (Fractional a, Ord a) => BinaryOp -> a -> a -> a
binaryOpToHaskellOperatorDouble op = case op of
  Plus -> (+)
  Minus -> (-)
  Multiply -> (*)
  Division -> (/)
  LogicAnd -> (\f s -> if f == 0 || s == 0 then 0 else 1)
  LogicOr -> (\f s -> if f /= 0 || s /= 0 then 1 else 0)
  EqualRelation -> (\l r -> if l == r then 1 else 0)
  NotEqualRelation -> (\l r -> if l /= r then 1 else 0)
  LessThanRelation -> (\l r -> if l < r then 1 else 0)
  LessEqualRelation -> (\l r -> if l <= r then 1 else 0)
  GreaterThanRelation -> (\l r -> if l > r then 1 else 0)
  GreaterEqualRelation -> (\l r -> if l >= r then 1 else 0)
  _ -> undefined