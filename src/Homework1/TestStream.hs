module Homework1.TestStream where

    data Stream a = Cons a (Stream a) deriving Show

    ruler :: Stream Integer
    ruler = ruler' 0
        where 
            ruler' x = interleave (repeat' x)(ruler' (x+1))


    repeat':: a->Stream a
    repeat' x = Cons x (repeat' x)

    --interleaveA
    {-
    interleave::Stream a -> Stream a -> Stream a
    interleave (Cons x xs)(Cons y ys) = Cons x (Cons y (interleave ys xs))
    -}

    --interleaveB
    interleave::Stream a -> Stream a -> Stream a
    interleave (Cons x xs) ys = Cons x (interleave ys xs)