--
-- Homework 2

--{-# OPTIONS_GHC -Wall #- }

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg  = case words msg of 
        ("I":t:s) -> LogMessage Info (read t::Int) ((unwords s) :: String)
        ("W":t:s) -> LogMessage Warning (read t::Int) ((unwords s) :: String)
        ("E":i:t:s) -> LogMessage (Error (read i::Int)) (read t::Int) ((unwords s) :: String)
        _           -> Unknown  msg
              

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

data MessageTree = Leaf
                | Node MessageTree LogMessage MessageTree

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
