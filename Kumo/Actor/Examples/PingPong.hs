module Kumo.Actor.Examples.PingPong where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async)
import qualified Data.ByteString.Char8    as C8 (pack, unpack)
import           Kumo.Actor               (Behavior, Context (..),
                                           Serializable (deserialize, serialize),
                                           host, remote, send, spawn,
                                           spawnRemote)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (getArgs)
import           Text.Printf              (printf)

data Msg = Ping Int | Pong Int deriving (Show, Read)

instance Serializable Msg where
    serialize = C8.pack . show
    deserialize = Just . read . C8.unpack

newtype Node = Node { _name :: String }

receive :: Behavior Msg Node
receive (Context self sender) (Node name) msg =
    print (name ++ " got " ++ show msg ++ " from " ++ show sender) >>
    case msg of
        Ping x -> send self sender (Pong $ x + 1)
        Pong x -> send self sender (Ping $ x + 1)

localExample :: IO ()
localExample = do
    p <- spawn receive (Node "p")
    q <- spawn receive (Node "q")
    send q p (Ping 0)
    sleep 10


distributiveExample :: IO ()
distributiveExample = do
    let ep = "http://127.0.0.1:3000/"
    pid <- spawnRemote ep receive (Node "p")
    let app = host pid
    async $ runWebApp 3000 app
    let pid = remote ep
    threadDelay 1000
    send pid pid (Ping 0)
    sleep 10


sleep n = threadDelay $ n * 1000000

runWebApp :: Int -> Application -> IO ()
runWebApp defaultPort app = fmap parsePort getArgs >>=
       logInfof (printf "http://localhost:%d/") >>=
       flip run app
  where parsePort [x] = read x :: Int
        parsePort _   = defaultPort

logInfof :: Show a => (a -> String) -> a -> IO a
logInfof fmt x = putStrLn ("[INFO] " ++ fmt x) >> return x
