-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Operation.hs                                       :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/04/03 12:50:42 by mayeung           #+#    #+#             --
--   Updated: 2025/06/13 16:37:29 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --

module Operation where

data UnaryOp =
  Complement
  | Negate
  | NotRelation
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
  deriving (Show, Eq)

varIdMapKey :: String
varIdMapKey = "#varid"