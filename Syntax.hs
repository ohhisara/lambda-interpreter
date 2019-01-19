-- Defina um data do Haskell para representar
--  termos do Lambda-calculus e implemente as 
--  funções substituição, redução beta e conversão 
--  de um termo à sua forma normal. Implemente as 
--  funções auxiliares que achar necessárias para 
--  a elaboração do trabalho. Escolha uma ordem de
--   redução fixa (ordem normal correspondente a 
-- call by name, ou ordem de aplicação correspondente 
-- a call by value).

module Syntax where


type Var = String

data Lambda = Variable Var
 | Abs Var Lambda
 | App Lambda Lambda
 --deriving(Show)

instance Show Lambda where
	show (Variable v) = v
	show (Abs v l) = "\\" ++ v ++ "." ++(show l)
	show (App l1 l2) = "("++ (show l1) ++(show l2) ++ ")"

