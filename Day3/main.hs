{-# LANGUAGE ScopedTypeVariables #-}

module Tim where

ex1 = [0,1,1,1,1,0]
ex2 = [1,1,0,0,1,0]


-- Function to count in one list test

count :: [Int] -> [(Int, Int)]
count xs = foldr f [(1,0) , (0,1)] xs
    where
        f 0 (x:(z, c):[]) = x:(z, (c+1)):[]
        f 1 ((o, c):x:[]) = (o, c+1):x:[]

getMaxCount :: [(Int, Int)] -> Int
getMaxCount xs = foldr (\x y -> if (snd x) > y then (fst x) else y) 0 xs

getMinCount :: [(Int, Int)] -> Int
getMinCount xs = foldr (\x y -> if (snd x) < y then (fst x) else y) 0 xs

transform :: Int -> [[Int]] -> [[Int]]
transform l [] = []
transform l xs = (map (\xs -> (xs !! l)) xs):(transform (l-1) xs)

-- We have to convert gamma and epsilon to decimal
toDecimal :: [Int] -> Int
toDecimal ps = go (length ps) ps
    where
        go l [] = 0
        go l (x:xs) = (x*(2^(l-1))) + (go (l-1) xs)


gamma :: [[Int]] -> [Int]
gamma xs = map getMaxCount $ (map count ) (transform (length xs) xs) 

epsilon :: [[Int]] -> [Int]
epsilon xs = map getMinCount $ (map count) (transform (length xs) xs)

cleaner :: String -> [[Int]]
cleaner = ((map (map read)).lines) 

main :: IO ()
main = 
    file <- cleaner <*> (readFile "input")
    let rFile :: [[Int]] = id file
    let g = gamma rFile
    let e = epsilon rFile
    let ans = (toDecimal g) * (toDecimal e)
    print ans
