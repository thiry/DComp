{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Mongoo where

import DB
import Measure

import Database.MongoDB
import Control.Monad.Trans (liftIO)

populate = do
 f <- readFile "titanic.dhs"
 let db = read f :: Graph
 let db'= insertMany "edges" (map (\(s,l,d) -> ["s" =: s, "l" =: l, "d" =: d]) db)
 pipe <- connect (host "127.0.0.1")
 e <- access pipe master "test" db'
 close pipe
 print e

query1 :: Action IO [Document]
query1 = rest =<< find (select ["l" =: "Sex","d" =: "male"] "edges")

prn ds = liftIO $ mapM_ (print . exclude ["_id"]) ds

run qry = do
 pipe <- connect (host "127.0.0.1")
 e <- access pipe master "test" (qry >>= prn)
 close pipe

