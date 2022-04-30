module Hw3 where


type Symbol = String
data Expr = Var Symbol | App Expr Expr | Lambda Symbol Expr deriving Eq

instance Show  Expr where
    show (Var s) = s
    show (App e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (Lambda s ex) = "(\\" ++ s ++ "." ++ (show ex) ++ ")"


variables :: [String]
variables = ["random" ++ (show n) | n <- [0..]]

testExpr :: Expr
testExpr = (App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "x"))


getFreeVars :: [Symbol] -> Expr -> [Symbol]
getFreeVars bounded (Var x) | x `elem` bounded = []
                            | otherwise = [x]
getFreeVars bounded (App exp1 exp2) = (getFreeVars bounded exp1) ++ (getFreeVars bounded exp2)
getFreeVars bounded (Lambda s exp) = (getFreeVars (bounded ++ [s]) exp)


getVars :: Expr -> [Symbol]
getVars = getFreeVars []

-- Integer for variables, symbol to dubstitute, symbol to substitute to, expression.
-- Return (new index, new expression)
substitute :: Int -> Symbol -> Expr -> Expr -> (Int, Expr)
substitute i from to (Var s) | s == from = (i, to)
                             | otherwise = (i, (Var s))

substitute i from to (App left right) = let leftRes = substitute i from to left
                                            rightRes = substitute (fst leftRes) from to right in
                                        ((fst rightRes), App (snd leftRes) (snd rightRes))


substitute i from to l@(Lambda s expr) | s == from = (i, l)
                                       | s `elem` (getVars to) = let substituted = substitute (i + 1) s (Var (variables !! i)) expr
                                                                     substituted' = substitute (fst substituted) from to (snd substituted) in
                                                        ((fst substituted'), (Lambda (variables !! i) (snd substituted'))) 
                                           | otherwise = let substituted = substitute i from to expr in
                                               (fst substituted, Lambda s (snd substituted))



first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a 

-- Is step already mare, int for random variable, Expression -> (int for random, was step made, new expr)
makeEvalStep :: Bool -> Int -> Expr -> (Bool, Int, Expr)
makeEvalStep True i ex = (True, i, ex)

makeEvalStep False i (Var symbol) = (False, i, (Var symbol))

makeEvalStep False i (Lambda s expr) = let evaledExpr = (makeEvalStep False i expr) in
                        ((first evaledExpr), (second evaledExpr), (Lambda s (third evaledExpr)))

makeEvalStep False i (App (Lambda s expr1) expr2) = let substituted = substitute i s expr2 expr1 in
                                        (True, (fst substituted), (snd substituted))

makeEvalStep False i (App expr1 expr2) = let evaledLeft = (makeEvalStep False i expr1)
                                             evaledRight = (makeEvalStep (first evaledLeft) (second evaledLeft) expr2) in
                            ((first evaledRight), (second evaledRight), (App (third evaledLeft) (third evaledRight)))


stepperEval :: (Bool, Int, Expr) -> (Bool, Int, Expr)
stepperEval (False, i, ex) = (False, i, ex)
stepperEval (True, i, ex) = (stepperEval (makeEvalStep False i ex))             

eval :: Expr -> Expr
eval ex = (third (stepperEval (True, 0, ex)))
