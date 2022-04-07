import Distribution.Simple.Utils (xargs)


separate :: [Int] -> ([Int], [Int])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = let (ods, evens) = separate xs
                    in (x:ods, y:evens)


chars :: String 
chars = "0123456789ABCDEF"

numToStr :: Int -> Int -> String 
numToStr 0 _ = ""
-- numToStr x s =  if s > 15 || s <= 0 then "" else numToStr (x `div` s) s ++ (chars !! (x `mod` s)):""
numToStr x s = if s > 15 || s <= 0 then "" else
    let d = x `div` s
        r = x `mod` s
    in
        numToStr d s ++ [chars !! r]


split :: Int -> [Int] -> [[Int]]
split _ [] = []
split n xs | length xs <= n = [xs]
           | otherwise = take n xs : split n (drop n xs)

averageN :: Int -> [Int] -> [Float]
averageN n xs = [fromIntegral (sum x) / fromIntegral (length x) | x <- xss] where xss = split n xs

-- Copy task

copy :: Integer -> String ->String
copy 0 _ = ""
copy n s = s ++ copy (n - 1) s

-- Using tail recursion

copyTail :: Integer -> String ->String 
copyTail n s = iter n s ""
    where iter num str acc | num <= 0 = acc
                           | otherwise = iter (num - 1) str (str ++ acc)

-- Luhn double
luhnDouble :: Int -> Int 
luhnDouble x | x > 4 = x * 2 - 9
             | otherwise = x * 2

luhn :: [Int] -> Bool 
luhn xs = let (rest, doubled) = separate (reverse xs) in
    (sum rest + sum (map luhnDouble doubled)) `mod` 10 == 0