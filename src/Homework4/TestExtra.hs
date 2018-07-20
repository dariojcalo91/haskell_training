module Homework4.TestExtra where

    funA :: a -> b -> a
    funA x _ = x

    funB :: a -> b -> b
    funB _ y = y

    funList :: a -> [a]
    funList a = [a] --funList a = a:[]
    funList' a = [a,a,a,a]
    funList'' a = []