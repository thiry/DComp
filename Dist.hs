module Dist where

import DB
import Measure

import Network.Socket.Internal (withSocketsDo)
import Network.Socket (close)
import Control.Concurrent (forkIO,threadDelay)
import Network -- (PortID(PortNumber),listenOn,accept,connectTo)
import System.IO (hGetLine,hPutStrLn,hClose,hFlush)
import Control.Monad (forever)

------------------------------------------------------
-- Optimizations / distribution
------------------------------------------------------
slave :: String -> PortNumber -> IO ()
slave file port = withSocketsDo $ do 
 dta <- readFile file
 let db = read dta :: Graph
 sock <- listenOn $ PortNumber port
 slavebody db sock

slavebody db sock = forever $ do
 (handle, host, port) <- accept sock
 query <- hGetLine handle
 let pat = read query :: Graph
 let r = answer pat db ctx
 hPutStrLn handle (show r)
 hFlush handle
 hClose handle

demo file basis = do 
 forkIO (slave file basis)
 forkIO (slave file (basis+10))
 threadDelay 1000000
 master' [("localhost",basis),("localhost",basis+10)] (show query)
 
-- master with n slaves
master' hosts q = withSocketsDo $ do
 let ns = map (\h -> connectTo (fst h) (PortNumber (snd h))) hosts
 r <- sequence (map (\n -> transmit n q) ns)
 let s = concat r
 p <- perf last s
 let m = show (snd $ last s) ++ " in " ++ (show p)
 putStrLn m

transmit n q = do
 h <- n
 hPutStrLn h q
 hFlush h
 rep <- hGetLine h
 let r = read rep :: [(Bool,Context)]
 hClose h
 return r

end n = do
 h <- n
 hClose h