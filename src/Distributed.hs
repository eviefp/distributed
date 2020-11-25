module Distributed where

import Prelude hiding
    (log)

import qualified CLI
import qualified Distributed.Protocol as P
import qualified Network.Simple.TCP   as N

run :: CLI.Options -> IO ()
run opts@CLI.Options { listen } =
    if listen
        then runServer opts
        else runClient opts

runServer :: CLI.Options -> IO ()
runServer o = N.serve N.HostAny (show $ CLI.port o) handler
  where
    handler :: (N.Socket, N.SockAddr) -> IO ()
    handler (socket, _) =
        N.recv socket 1000 >>=
            (\mb -> print $ mb >>= P.decode)

runClient :: CLI.Options -> IO ()
runClient o = N.connect "localhost" (show $ CLI.port o) handler
  where
    handler :: (N.Socket, N.SockAddr) -> IO ()
    handler (socket, _) = N.sendLazy socket $ P.encode (connect o)

connect :: CLI.Options -> P.Message
connect o = P.Connect $ P.NodeName $ CLI.name o

log :: CLI.Options -> String -> IO ()
log o message = putStrLn $ "<" <> CLI.name o <> "> " <> message
