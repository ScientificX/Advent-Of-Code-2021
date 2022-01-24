{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import Control.Monad

radar :: [Int]
radar = [1,2,3,4,5,6]

solution' :: [Int] -> (Int, Int)
solution' = foldr f (0,0)
    where
        f x (curr, k) = if curr > x then (x, k+1) else (x, k)

solution xs = snd $ solution' xs

part1 = do
    list <- lines <$> readFile "input" 
    let list' :: [Int] = map read list
    let soln = solution list'
    print soln


-- Part 2

sumw :: [(Int, Int, Int)] -> [Int]
sumw xss = map f xss
  where
    f (a,b,c) = a+b+c


f xs = zip3 xs (tail xs) (tail $ tail xs)

solution2 xs = solution $ sumw $ f xs


part2 = do
  list <- lines <$> readFile "input"
  let list' :: [Int] = map read list
  let soln = solution2 list'
  print soln


main :: IO ()
main = undefined
