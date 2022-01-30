{-# LANGUAGE ScopedTypeVariables #-}

module Main where

cleaner :: String -> [(String, String)]
cleaner = (map (span (/= ' ')).lines)

-- ("depth", "horizontal position")

solution1' :: [(String, String)] -> Int
solution1' xs = p $ foldr f v xs
  where
    p (a,b) = a*b
    v = (0, 0)
    f as@(a, d) bs@(b, e) | a == "forward" = let x = read d :: Int in (b, (e+x))
                          | a == "down" = let x = read d :: Int in ((b-x), e)
                          | a == "up" = let x = read d :: Int in ((b+x), e)
                          | otherwise = undefined

solution1 :: IO ()
solution1 = do
  file <- cleaner <$> (readFile "input")
  let list' :: [(String, String)] = map id file
  let list = solution1' list'
  print list

main :: IO ()
main = undefined
