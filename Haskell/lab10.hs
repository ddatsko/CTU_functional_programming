interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys): map (y:) (interleave x ys)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [interleave x l | l <- permutations xs]


type Edge a = (a,a)
data Graph a = Graph {vertices :: [a], edges :: [Edge a]} deriving Show

gr :: Graph Int
gr = Graph {vertices=[1..6], edges=[(1, 2), (1, 5), (2, 3), (2, 5), (3, 4), (4, 5), (4, 6)]}


isHamPath :: Eq a => Graph a -> [a] -> Bool
isHamPath g p = all (\pair -> (pair `elem` edges g) || ((snd pair, fst pair) `elem` edges g)) (zip p (tail p))

findHamPath :: Eq a => Graph a -> [a]
findHamPath g = let paths = filter (isHamPath g) (permutations (vertices g)) in
    if null paths then
        []
    else
        head paths


data DualNum a = DN a a deriving (Eq, Ord)
instance Show a => Show (DualNum a) where
    show (DN x x') = show x ++ " + " ++ show x' ++ "eps"

instance Num a => Num (DualNum a) where
    (DN x x') + (DN y y') = DN (x + y) (x' + y')
    (DN x x') - (DN y y') = DN (x - y) (x' - y')
    (DN x x') * (DN y y') = DN (x * y) (x*y' + y*x')
    fromInteger i = DN (fromInteger i) 0
    abs (DN x x') = DN (abs x) (signum x * x')
    signum (DN x _) = DN (signum x) 0

instance Fractional a => Fractional (DualNum a) where
    (DN x x') / (DN y y') = DN (x/y) ((x'*y - x*y') / (y*y))
    fromRational r = DN (fromRational r) 0



f :: Num a => a -> a
f x = x^2 + 1
