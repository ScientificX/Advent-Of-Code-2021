module Main where

cleaner :: String -> [(String, String)]
cleaner = (map (span (/= ' ')).lines)

-- ("depth", "horizontal position")

solution1 :: [(String, String)] -> Int
solution1 xs = p $ foldr f v xs
  where
    p (a,b) = a*b
    v = (0, 0)
    f as@(a, d) bs@(b, e) | a == "forward" = let x = read d :: Int in (b, (e+x))
                          | a == "down" = let x = read d :: Int in ((b-x), e)
                          | a == "up" = let x = read d :: Int in ((b+x), e)
                          | otherwise = undefined

main :: IO ()
main = undefined
