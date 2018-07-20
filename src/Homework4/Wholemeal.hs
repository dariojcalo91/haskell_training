----------------------------------------------------------------------
-- dcalo
-- 20/02/2018
-- CIS194 
--
-- CIS 194 Spring 2013: Homework 04
--
----------------------------------------------------------------------

module Homework4.Wholemeal where

    ----------------------------------------------------------------------
    -- Exercise 1
    ----------------------------------------------------------------------
    
    fun1 :: [Integer] -> Integer
    fun1 []     = 1
    fun1 (x:xs)
        | even x    = (x - 2) * fun1 xs
        | otherwise = fun1 xs
    
    fun2 :: Integer -> Integer
    fun2 1 = 0
    fun2 n
      | even n    = n + fun2 (n `div` 2)
      | otherwise = fun2 (3*n + 1)
    
    -- |
    --
    -- >>> fun1 [1,3,5,7] == fun1' [1,3,5,7]
    -- True
    -- >>> fun1 [1,2,3] /= fun1' [1,2,3]
    -- False
    -- >>> fun2 10 == fun2' 10
    -- True
    -- >>> fun2 15 /= fun2' 15
    -- False
    
    fun1' :: [Integer] -> Integer
    fun1' = product .  map (\x -> x-2) . filter even
    
    fun2' :: Integer -> Integer
    fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3*x+1)
   
    ----------------------------------------------------------------------
    -- Exercise 2
    ----------------------------------------------------------------------
   
    data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq, Ord)

    treeLevel :: Tree a -> Integer
    treeLevel Leaf           = 0
    treeLevel (Node n _ _ _) = n

    foldTree :: (Ord a) => [a] -> Tree a
    foldTree = foldr treeInsert Leaf
        where
            treeInsert x Leaf = Node 0 Leaf x Leaf
            treeInsert x (Node n left root right)
                | left > right = Node (treeLevel newRight + 1) left root newRight
                | otherwise = Node (treeLevel newLeft + 1) newLeft root right
                    where
                    newRight = treeInsert x right
                    newLeft = treeInsert x left
   
    ----------------------------------------------------------------------
    -- Exercise 3
    ----------------------------------------------------------------------
    
    -- |
    --
    -- >>> xor [False, True, False]
    -- True
    -- >>> xor [False, True, False, False, True]
    -- False
    
    xor :: [Bool] -> Bool
    xor = undefined
    
    -- |
    --
    -- >>> map' (+1) [1,2,3]
    -- [2,3,4]
    
    map' :: (a -> b) -> [a] -> [b]
    map' = undefined
    
    -- Optional
    
    myFoldl :: (a -> b -> a) -> a -> [b] -> a
    myFoldl = undefined
    
    ----------------------------------------------------------------------
    -- Exercise 4
    ----------------------------------------------------------------------
    
    sieveSundaram :: Integer -> [Integer]
    sieveSundaram = undefined