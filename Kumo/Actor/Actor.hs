module Kumo.Actor.Actor
    ( Behavior
    , Context(..)
    , spawn
    , spawnRemote
    ) where

import           Control.Concurrent.Async     (async)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (newTChanIO, readTChan)
import           Control.Monad                (forever)
import           Kumo.Actor.Exception         (logException)
import           Kumo.Actor.Pid               (Pid, local, remote)

data Context m = Context { self   :: Pid m
                         , sender :: Pid m
                         }

type Behavior m a = Context m -> a -> m -> IO ()

spawn :: Behavior m a -> a -> IO (Pid m)
spawn receive state = do
    mailbox <- newTChanIO
    let self = local mailbox
    forwarding mailbox self receive state
    return self

spawnRemote :: String -> Behavior m a -> a -> IO (Pid m)
spawnRemote ep receive state = do
    mailbox <- newTChanIO
    forwarding mailbox (remote ep) receive state
    return $ local mailbox

forwarding mailbox self receive state = async $ forever $ logException $ do
    (sender, msg) <- atomically (readTChan mailbox)
    receive (Context self sender) state msg
