module TautologyChecker where 
data Prop = Const Bool 
            | Var Char 
            | Not Prop 
            | And Prop Prop 
            | Imply Prop Prop 
            | Or Prop Prop 
type Assoc a b = [(a, b)]
type Environment = Assoc Char Bool 

find :: Char -> Environment -> Bool
find name [] = undefined 
find name ((k , v) : subEnv) = if k == name then v else find name subEnv

evalProp :: Prop -> Environment -> Bool
evalProp (Const value) env = value 
evalProp (Var name) env = find name env 
evalProp (Not subProp) env = not $ evalProp subProp env 
evalProp (And p1 p2) env = (evalProp p1 env) && (evalProp p2 env)
evalProp (Imply p1 p2) env = (evalProp p1 env) <= (evalProp p2 env)
evalProp (Or p1 p2) env = (evalProp p1 env) || (evalProp p2 env)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2 
vars (Imply p1 p2) = vars p1 ++ vars p2 
vars (Or p1 p2) = vars p1 ++ vars p2 

bools 0 = [[]]
bools length = map (False : ) subBools ++ map (True :) subBools 
                where subBools = bools (length - 1)

removeDuplicates [] = []
removeDuplicates (x:xs) = x : (removeDuplicates (filter (/= x) xs))

environments :: Prop -> [Environment]
environments p = map (zip vs) (bools (length vs)) 
                  where vs = removeDuplicates (vars p)

isTautology p = and [evalProp p env | env <- environments p]
