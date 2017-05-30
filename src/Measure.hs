module Measure where

import Data.Time (getCurrentTime,diffUTCTime)
import Control.Exception.Base (evaluate)

------------------------------------------------------
-- Performance measurement: time complexity
------------------------------------------------------

perf f x = do
 let fx = f x 
 t1 <- getCurrentTime
 evaluate fx
 t2 <- getCurrentTime
 return (diffUTCTime t2 t1,fx)

perfs f xs = map (perf f) xs
