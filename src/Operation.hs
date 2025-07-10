-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Operation.hs                                       :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:50:42 by mayeung           #+#    #+#             --
--   Updated: 2025/07/10 17:15:21 by mayeung          ###   ########.fr       --
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

unaryOpToHaskellOperator :: (Num a, Bits a) => UnaryOp -> a -> a
unaryOpToHaskellOperator op = case op of
  Complement -> complement
  Negate -> negate
  UPlus -> id
  NotRelation -> (\u -> if popCount u == 0 then 1 else 0)
  _ -> undefined

binaryOpToHaskellOperator :: (Integral a, Bits a) => BinaryOp -> a -> a -> a
binaryOpToHaskellOperator op = case op of
  Plus -> (+)
  Minus -> (-)
  Multiply -> (*)
  Division -> (\l r -> fst (quotRem l r))
  Modulo -> (\l r -> snd (quotRem l r))
  BitOr -> (.|.)
  BitAnd -> (.&.)
  BitXor -> xor
  BitShiftLeft -> (\i n -> shiftL i (fromIntegral n))
  BitShiftRight -> (\i n -> shiftR i (fromIntegral n))
  LogicAnd -> (\f s -> if popCount f == 0 || popCount s == 0 then 0 else 1)
  LogicOr -> (\f s -> if popCount f /= 0 || popCount s /= 0 then 1 else 0)
  EqualRelation -> (\l r -> if l == r then 1 else 0)
  NotEqualRelation -> (\l r -> if l /= r then 1 else 0)
  LessThanRelation -> (\l r -> if l < r then 1 else 0)
  LessEqualRelation -> (\l r -> if l <= r then 1 else 0)
  GreaterThanRelation -> (\l r -> if l > r then 1 else 0)
  GreaterEqualRelation -> (\l r -> if l >= r then 1 else 0)
  _ -> undefined
