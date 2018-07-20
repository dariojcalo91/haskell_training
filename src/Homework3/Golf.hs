----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------

module Homework3.Golf where

    ----------------------------------------------------------------------
    -- Exercise 1
    ----------------------------------------------------------------------
    
    -- |
    --
    -- >>> skips "ABCD"
    -- ["ABCD","BD","C","D"]
    -- >>> skips "hello!"
    -- ["hello!","el!","l!","l","o","!"]
    -- >>> skips [1]
    -- [[1]]
    -- >>> skips [True, False]
    -- [[True,False],[False]]
    -- >>> skips []
    -- []

    skips :: [a] -> [[a]]
    --skips lst = [getNth lst i | i<-[1..length lst]]
    skips lst = map (getNth lst) [1..length lst]
    
    getNth :: [a] -> Int -> [a]
    --getNth lst n = [lst !! n | n <- [n-1, n-1+n..length lst -1]]
    getNth lst n = map (lst !!) [n-1, n-1+n..length lst -1]

    ----------------------------------------------------------------------
    -- Exercise 2
    ----------------------------------------------------------------------
    
    -- |
    --
    -- >>> localMaxima [2,9,5,6,1]
    -- [9,6]
    -- >>> localMaxima [2,3,4,1,5]
    -- [4]
    -- >>> localMaxima [1,2,3,4,5]
    -- []
    
    localMaxima :: [Integer] -> [Integer]
    localMaxima (x1:x2:x3:xs) 
        | x1<x2 && x2>x3  =  x2 : localMaxima (x3:xs)
        | otherwise   =  localMaxima (x2:x3:xs)
    localMaxima _ = []
    
    ----------------------------------------------------------------------
    -- Exercise 3
    ------------------ anonymous function----------------------------------------------------
    
    -- |
    --
    -- >>> histogram [1,1,1,5]
    -- " *        \n *        \n *   *    \n==========\n0123456789\n"

    histogram :: [Integer] -> String
    histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
      where 
        c = count xs
        m = maximum c

    -- returns one * line from the above function
    line :: [Int] -> Int -> String
    line (x:xs) n 
        | x >= n = "*" ++ line xs n 
        | otherwise = "-" ++ line xs n
    line [] n = ""

    -- counts occurence of numbers in [0..9] in the input list.
    count :: [Integer] -> [Int]
    count xs = map (\n -> length $ filter (== n) xs) [0..9]