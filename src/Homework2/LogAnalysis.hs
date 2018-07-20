{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module Homework2.LogAnalysis where

import Homework2.Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------
 
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage logmsg = case words logmsg of
  ("I" : timestamp : xs) -> LogMessage Info (read timestamp) (unwords xs)
  ("W" : timestamp : xs) -> LogMessage Warning (read timestamp) (unwords xs)
  ("E" : level : timestamp : xs) -> LogMessage (Error $ read level) (read timestamp) (unwords xs)
  msg -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse logmsg = parseMessage <$> lines logmsg

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- Insert
-- 
-- >>>
--
insert :: LogMessage -> MessageTree -> MessageTree
insert logmsg@LogMessage{} Leaf = Node Leaf logmsg Leaf --corrector sugiere  {} en lugar de  LogMessage _ _ _
insert logmsg1@(LogMessage _ ts1 _) (Node left logmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left logmsg2 (insert logmsg1 right)
  | otherwise = Node (insert logmsg1 left) logmsg2 right
insert _ tree = tree --unknow no cambia el arbol

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- Insert from [LogMessage] to Tree
--
-- >>>
--

build :: [LogMessage] -> MessageTree
--build [] = Leaf
--build (lmsg1:lmsg2) = insert lmsg1 (build lmsg2)
build = foldr insert Leaf --sug. hlint
----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- take a soted MessageTree and produce a list of all the LogMessages
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logmsg right) = inOrder left ++ [logmsg] ++ inOrder right

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong   = getMessage . inOrder . build . filter validateMoreThan

validateMoreThan :: LogMessage -> Bool
validateMoreThan (LogMessage (Error num) _ _) 
  | num > 50 = True
  | otherwise = False
validateMoreThan _ = False

getMessage :: [LogMessage] -> [String]
getMessage (LogMessage _ _ msg : msgs) = msg : getMessage msgs
getMessage _ = []

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

--whoDidIt :: String
--whoDidIt = undefinedts