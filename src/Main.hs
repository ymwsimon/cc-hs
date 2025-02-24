-- ************************************************************************** --
--                                                                            --
--                                                        :::      ::::::::   --
--   Main.hs                                            :+:      :+:    :+:   --
--                                                    +:+ +:+         +:+     --
--   By: mayeung <mayeung@student.42london.com>     +#+  +:+       +#+        --
--                                                +#+#+#+#+#+   +#+           --
--   Created: 2025/02/24 00:05:21 by mayeung           #+#    #+#             --
--   Updated: 2025/02/24 10:54:18 by mayeung          ###   ########.fr       --
--                                                                            --
-- ************************************************************************** --


module Main where

import Options.Applicative

data  Args = Args {ifiles :: [String], codegen :: Bool, codeparse :: Bool}

main :: IO ()
main = putStrLn "Hello, Haskell!"
