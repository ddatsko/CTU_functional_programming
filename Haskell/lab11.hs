toUpper :: Char -> Char
toUpper c = case lookup c $ zip ['a'..'z'] ['A'..'Z'] of
    Just c' -> c'
    Nothing -> c

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x):xs

toCamelCase :: String -> String
toCamelCase s = concat $ map capitalize (words s) 


toCamelCaseF :: Functor f => f String -> f String
toCamelCaseF = fmap toCamelCase



data State = Before | Digit | Dot | First | Second | Fail deriving Show

isNum :: Char -> Bool
isNum c = c `elem` ['0'..'9']

final :: State -> Bool
final Second = True
final _ = False

delta :: State -> Char -> State

delta Fail _ = Fail

delta Before c | isNum c = Digit
               | otherwise = Fail

delta Digit c | isNum c = Digit
              | c == '.' = Dot
              | otherwise = Fail

delta Dot c | isNum c = First
            | otherwise = Fail

delta First c | isNum c = Second
              | otherwise = Fail

delta Second _ = Fail


data DFA a = Automaton (a->Char->a) a (a->Bool)

automaton :: DFA State
automaton = Automaton delta Before final

evalDFA :: DFA a -> String -> Bool
evalDFA (Automaton dlt s isF) w = isF (foldl dlt s w)

parseNum :: String -> Maybe Float
parseNum w = if evalDFA automaton w then Just (read w)
             else Nothing

parseNumF :: Functor f => f String -> f (Maybe Float)
parseNumF = fmap parseNum


parseIO :: IO ()
parseIO = putStrLn "Enter a number: " >> parseNumF getLine >>= \x -> case x of 
    Just _ -> putStrLn "OK"
    Nothing -> parseIO

data Expr a = Atom a
              | Neg (Expr a)
              | And (Expr a) (Expr a) 
              | Or (Expr a) (Expr a) 
                deriving (Eq, Show)

expr :: Expr Bool
expr = Or (And (Atom True) (Neg (Atom False))) (Atom False) 
fle :: Expr String
fle = And (Or (Neg (Atom "y")) (Atom "x")) (Atom "y")

evalExpr :: Expr Bool -> Bool
evalExpr (Atom a) = a
evalExpr (Neg ex) = not $ evalExpr ex
evalExpr (And ex1 ex2) = (evalExpr ex1) && (evalExpr ex2)
evalExpr (Or ex1 ex2) = (evalExpr ex1) || (evalExpr ex2)


getAtoms :: Expr a -> [a]
getAtoms (Atom x) = [x]
getAtoms (Neg ex) = getAtoms ex
getAtoms (And ex1 ex2) = (getAtoms ex1) ++ (getAtoms ex2)
getAtoms (Or ex1 ex2) = getAtoms (And ex1 ex2)

instance Functor Expr where
    fmap f (Atom c) = Atom (f c)
    fmap f (Neg e) = Neg (fmap f e)
    fmap f (And e1 e2) = And (fmap f e1) (fmap f e2)
    fmap f (Or e1 e2) = Or (fmap f e1) (fmap f e2)

subst :: Functor f => [String] -> f String -> f Bool
subst trues s = fmap (\x -> x `elem` trues) s


subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x:xs) = subseqs xs ++ [x:ys | ys <- subseqs xs]


isSat :: Expr String -> Bool
isSat ex = or $ map (\perm -> evalExpr (subst perm ex)) (subseqs (getAtoms ex))

isTaut :: Expr String -> Bool
isTaut ex = and $ map (\perm -> evalExpr (subst perm ex)) (subseqs (getAtoms ex))
