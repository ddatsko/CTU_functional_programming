data Tree a = Leaf a | Node (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
    show (Leaf x) = "<Leaf '" ++ show x ++ "'/>"
    show (Node x y) = "<Node>" ++ show x ++ ", " ++ show y ++ "</Node>"


tree :: Tree Char
tree = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'x'))

treeDepth :: Tree a -> Int 
treeDepth (Leaf x) = 1
treeDepth (Node l r) = 1 + max (treeDepth l) (treeDepth r)


labelIter2 :: Tree a -> Int -> (Tree (a, Int), Int)
labelIter2 (Leaf x) n = (Leaf (x, n), n + 1)
labelIter2 (Node x y) n = ((Node (fst l) (fst r)), (snd l) + (snd r)) where 
    l = labelIter2 x n
    r = labelIter2 y (snd l)



labelTree :: Tree a -> Tree (a, Int)
labelTree x = fst (labelIter2 x 0)

-- Tasks
type Monomial a = (a, Int)

data Polynomial a = Null | Pol (Monomial a) (Polynomial a)

instance (Show a) => Show (Polynomial a) where
    show Null = "0"
    show (Pol (m, 0) pol) = "(" ++ show m ++ ") + " ++ show pol
    show (Pol (m, e) pol) = "(" ++ show m ++ ")*x^" ++ show e ++ " + " ++ show pol


getDegree :: Polynomial a -> Int 
getDegree Null = -1
getDegree (Pol (c, e) p) = max e (getDegree p) 
