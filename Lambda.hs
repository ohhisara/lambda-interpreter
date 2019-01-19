import Syntax

normalize::Lambda -> Lambda
normalize (Variable v) = (Variable v)
normalize (App l1 l2) = beta (App (normalize l1) (normalize l2))
normalize (Abs v l1) = (Abs v (normalize l1))

beta::Lambda->Lambda
beta (App (Abs v l1) l2) 
	| ((getInterception l1 l2)==[]) = (normalize (subst l1 (v,l2)))
	| otherwise = normalize (subst (alphaConvs (getInterception l1 l2) l1) (v, l2))
beta l = l

subst:: Lambda -> (Var, Lambda) -> Lambda
subst (Variable v) (v1,l) 
	| v == v1 = l
	| otherwise = (Variable v)
subst (App l1 l2) (v,l)= (App (subst l1 (v,l)) (subst l2 (v,l)))
subst (Abs x l) (v, l1) = (Abs x (subst l (v,l1)))

-- lista de variaveis pertencentes à interceção de BV(l1) e FV(l2)
getInterception::Lambda->Lambda->[Var]
getInterception l1 l2 = (intercept (boundVar l1) (freeVars l2 []))

-- interceção de duas listas de variaveis
intercept::[Var] -> [Var] -> [Var]
intercept [] l1 = []
intercept l1 [] = []
intercept (v:l1) l2 
	| v `elem` l2 = v:(intercept l1 l2)
	| otherwise = intercept l1 l2

-- lista de variaveis livres de um lambda
freeVars::Lambda -> [Var]->[Var]
freeVars (Variable v) list
	| v `elem` list = []
	| otherwise = [v]
freeVars (Abs v l1) list 
	| v `elem` list = (freeVars l1 list)
	|otherwise = 
		let list1 =(v:list)
		in freeVars l1 list1
freeVars (App l1 l2) list= (freeVars l1 list)++(freeVars l2 list)

-- lista de variaveis bound de um lambda
boundVar:: Lambda -> [Var]
boundVar (Variable x) = []
boundVar (Abs x lambda) = x:(boundVar lambda)
boundVar (App l1 l2) = (boundVar l1)++(boundVar l2)

-- conversao alpha de uma lista de variaveis
alphaConvs::[Var] ->Lambda ->Lambda
alphaConvs [] l = l
alphaConvs (v:vs) l=  alphaConvs vs (alphaConv v l)

-- conversao alpha de uma variavel
alphaConv::Var->Lambda->Lambda
alphaConv v (Variable v1) 
	|v==v1 = (Variable (v++"'"))
	|otherwise = (Variable v1)
alphaConv v (Abs v1 l1)
	|v==v1 = (Abs (v++"'") (alphaConv v l1))
	|otherwise = (Abs v1 (alphaConv v l1))
alphaConv v (App l1 l2) = (App (alphaConv v l1) (alphaConv v l2))
 
pp::Lambda -> String
pp (Variable v) = v
pp (Abs v l) = "\\" ++ v ++ "." ++(pp l)
pp (App l1 l2) = "(" ++ pp l1 ++ pp l2 ++ ")"

out::Lambda -> String
out l = pp (normalize l)

--CHURCH
zero::Lambda
zero = (Abs "f" (Abs "x" (Variable "x")))

one::Lambda
one = (Abs "f" (Abs "x" (App (Variable "f") (Variable "x"))))

two::Lambda
two=(Abs "f" (Abs "x" (App (Variable "f") (App (Variable "f") (Variable "x")))))

three::Lambda
three = (Abs "f" (Abs "x" (App (Variable "f") (App (Variable "f") (App (Variable "f") (Variable "x"))))))

plus::Lambda->Lambda->Lambda
plus m n = normalize (Abs "f" (Abs "x" (App (App m (Variable "f")) (App (App n (Variable "f")) (Variable "x")))))

succc::Lambda->Lambda
succc n = normalize (Abs "f" (Abs "x" (App (Variable "f") (App (App n (Variable "f")) (Variable "x")))))

mult::Lambda->Lambda->Lambda
mult m n = normalize (Abs "f" (App m (App n (Variable "f"))))

expo::Lambda->Lambda->Lambda
expo m n = normalize (Abs "f" (Abs "x" (App (App (App n m) (Variable "f")) (Variable "x"))))

--Exemplos
-- (\x.x)((\y.y)z)
ex = App (App (Abs "x" (Variable "x")) (Abs "y" (Variable "y"))) (Variable "z")

-- (\x.xx)(\x.xx)
ex1 = App (Abs "x" (App (Variable "x") (Variable "x"))) (Abs "x" (App (Variable "x") (Variable "x")))

-- \x.((\y.y)2)
ex2 = Abs "x" (App (Abs "y" (Variable "y")) (Variable "2"))

-- \x.((\y.xy)2)
ex3 = Abs "x" (App (Abs "y" (App (Variable "x") (Variable "y"))) (Variable "2"))

-- (\x.((\y.xy)2))1
ex4 = App (ex3) (Variable "1")

-- \xy.yx(ab)
ex5 = App (Abs "y" (App (Abs "x" (App (Variable "y") (Variable "x"))) (Variable "a"))) (Variable "b")

-- (\x.\y.xy)y 
ex6 = App (Abs "x" (Abs "y" (App (Variable "x") (Variable "y")))) (Variable "y") 

-- (\x.\x.x)x
ex7 = App (Abs "x" (Abs "x" (Variable "x"))) (Variable "x")

-- (\x.(\z.z)(x))(\y.zy)
ex8 = App (Abs "x" (App (Abs "z" (Variable "z")) (Variable "x"))) (Abs "y" (App (Variable "z") (Variable "y"))) --devia dar erro?

-- ((\x.\y.xy)(\x.xy))(\a.\b.ab)
ex9= App (App (Abs "x" (Abs "y" (App (Variable "x") (Variable "y")))) (Abs "x" (App (Variable "x") (Variable "y")))) (Abs "a" (Abs "b" (App (Variable "a") (Variable "b"))))

 -- (\x.xy)y
ex10= App (Abs "x" (App (Variable "x") (Variable "y"))) (Variable "y")