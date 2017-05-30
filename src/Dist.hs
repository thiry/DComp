module Dist where

import Context
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
 let r = answer pat db []
 hPutStrLn handle (show r)
 hFlush handle
 hClose handle

-- master with n slaves
master' hosts q = withSocketsDo $ do
 let ns = map (\h -> connectTo (fst h) (PortNumber (snd h))) hosts
 r <- sequence (map (\n -> transmit n q) ns)
 p <- perf concat r
 let m = "Get result in " ++ (show $ fst p)
 putStrLn m

master'' hosts q = withSocketsDo $ do
 let ns = map (\h -> connectTo (fst h) (PortNumber (snd h))) hosts
 r <- sequence (map (\n -> transmit n q) ns)
 p <- perf concat r
 print (map snd (snd p))

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
