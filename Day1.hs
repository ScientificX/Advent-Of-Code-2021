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

main = do
    list <- lines <$> readFile "input" 
    let list' :: [Int] = map read list
    let soln = solution list'
    print soln