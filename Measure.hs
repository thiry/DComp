module Measure where

import Data.Time (getCurrentTime,diffUTCTime)
import Control.Exception.Base (evaluate)

------------------------------------------------------
-- Performance measurement: time complexity
------------------------------------------------------

perf f x = do
 t1 <- getCurrentTime
 evaluate (f x)
 t2 <- getCurrentTime
 return (diffUTCTime t2 t1)

perfs f xs = map (perf f) xs
