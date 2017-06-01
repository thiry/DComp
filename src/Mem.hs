module Mem where

import Context
import Query
import DB

import System.Process (callCommand)
import Data.Char (isUpper)

import Data.Time (getCurrentTime,diffUTCTime)
import Control.Exception.Base (evaluate)

--------------------
--- memorization ---
--------------------
type Mem = [(Pattern,Result)]

ctn :: Pattern -> Mem -> Bool
ctn x []         = False
ctn x ((p,v):cs) = if null w then ctn x cs else True
 where w = answer p x []

gt :: Pattern -> Mem -> Result
gt x ((p,v):cs) = if null w then gt x cs else v
 where w = answer p x []

pt :: Pattern -> Result -> Mem -> Mem
pt k v cs = (k,v):cs

answer' :: Mem -> Pattern -> Graph -> (Mem,Result)
answer' mem ps vs = if ctn ps mem then (mem,gt ps mem) else (mem',rs)
 where rs   = answer ps vs []
       mem' = pt ps rs mem

rx = answer' [] [("?x","a","b")] [("z","a","e"),("z","a","b"),("e","f","g")]
ry = answer' [([("?x","a","b")],[(True,[("?x","k")])])] [("?x","a","b")] []

test = do
 db <- readFile "titanic.dhs"
 let graph = read db :: Graph
 let query1 = query "(?X Sex male)"
 let query2 = query "(?X Sex male) and (?X Survived 1)"
 let query3 = query "(?X ?Y ?Z)"
 let mem0 = []
 let (mem1,r1) = answer' mem0 query1 graph
 t1 <- getCurrentTime
 evaluate r1
 t2 <- getCurrentTime
 putStrLn ("For q1")
 putStrLn ("-Without memorization: "++show (diffUTCTime t2 t1))
 let (mem2,r2) = answer' mem1 query1 graph
 evaluate r2
 t3 <- getCurrentTime  
 putStrLn ("-With memorization: "++show (diffUTCTime t3 t2))
 let (mem3,r3) = answer' mem0 query2 graph
 t4 <- getCurrentTime
 evaluate r3
 t5 <- getCurrentTime
 putStrLn ("For q2")
 putStrLn ("-Without memorization: "++show (diffUTCTime t5 t4))
 let (mem4,r4) = answer' mem3 query2 graph
 evaluate r4
 t6 <- getCurrentTime  
 putStrLn ("-With memorization: "++show (diffUTCTime t6 t5))
 let (mem5,r5) = answer' mem0 query3 graph
 t7 <- getCurrentTime
 evaluate r5
 t8 <- getCurrentTime
 putStrLn ("For q3")
 putStrLn ("-Without memorization: "++show (diffUTCTime t8 t7))
 let (mem6,r6) = answer' mem5 query3 graph
 evaluate r6
 t9 <- getCurrentTime  
 putStrLn ("-With memorization: "++show (diffUTCTime t9 t8))












