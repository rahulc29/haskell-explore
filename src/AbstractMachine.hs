module AbstractMachine where
data Expr = Val Int 
            | Add Expr Expr 

type ControlStack = [Op]
data Op = EVAL Expr | ADD Int 

eval (Val n) c = exec c n 
eval (Add x y) c = eval x (EVAL y : c)

exec [] n = n 
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)