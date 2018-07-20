----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Homework7.JoinList where

import Homework7.Sized
import Data.Monoid
import Homework7.Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b --The most important operation is how to append

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------
--definend Size type class, Size type and method to get size
--for Append need the Size (use() tag)

tagSize ::(Sized a, Monoid a) => JoinList a b -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i<0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i  (Append _ ls1 ls2)
  | i >= tagSize ls1 = indexJ (i-tagSize ls1) ls2
  | otherwise        = indexJ i ls1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty         = Empty
dropJ i jl |i <= 0    = jl
dropJ _ (Single _ _)  = Empty
dropJ i (Append _ left right)
  | i < tagSize left = dropJ i left +++ right
  | otherwise = dropJ (i - tagSize left) right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty      = Empty
takeJ i _ | i <= 0 = Empty
takeJ _ jl@(Single _ _) = jl
takeJ i (Append _ left right)
  | i > tagSize left = left +++ takeJ (i - tagSize left) right
  | otherwise = takeJ i left

--consider a safe list indexing function
(!!?) :: [a] -> Int -> Maybe a
[]     !!?   _           = Nothing
_      !!?   i | i <0    = Nothing
(x:xs) !!?   0           = Just x
(x:xs) !!?   i           = xs !!? (i-1)

--consider an updated function for converting
--join-list into list
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++jlToList l2
----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------
scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------


----------------------------------------------------------------------
-- Extra practice
----------------------------------------------------------------------
-- similar a maybe a
data Optional a = Nada | Only a
  deriving(Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  Nada `mappend` m = m
  m `mappend` Nada = m
  (Only m1) `mappend` (Only m2) = Only (m1 `mappend` m2)

-- template of phrases
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String

madlibbin' e adv noun adj =
  e
  <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."
--Now you’re going to refactor this code a bit! Rewrite it using
--mconcat.

madlibbin'' e1 adv1 noun1 adj1 = 
  mconcat [e1,"! he said ", adv1, " as he jumped into his car ", noun1, " and drove off with his ", adj1, " wife."]

lst :: [Integer]
lst = [1,5,8,23,423,99]

prod'::Integer
prod' = getProduct . mconcat . map Product $ lst