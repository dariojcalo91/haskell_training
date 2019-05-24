module Homework1.Extra where

    --find the last element of list
    --try [1,2,3,4] & ['x','y','z']
    myLast::[a] -> a
    myLast [] = error "No ult. elem. para lista vacia!"
    myLast [x] = x
    myLast (_:xs) = myLast xs --recursivo

    --find the last but one element of a list
    --try [1,2,3,4] & ['a'..'z']
    myButLast::[a] -> a
    myButLast = last . init --usando composicion de funciones / parametros implicitos

    --the k-th element of a list
    elementAt :: [a]->Int->a
    elementAt [] _ = error "datos erroneos!"
    elementAt n x
        | length n < x     = error "el numero es mayor que la longitud de la lista!"
        | length n == x     = last n
        | otherwise         = elementAt (init n) x

    