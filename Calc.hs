--
--
--Homework 5
--

module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

--evalStr :: String -> Maybe Integer
