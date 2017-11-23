{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Kumo.Actor.Actor
    ( Actor(..)
    , Behavior
    , Context(..)
    , spawn
    , spawnRemote
    , spawn_
    , spawnRemote_
    ) where

import           Control.Concurrent.Async     (Async, async)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan)
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

class Actor a m | a -> m where
    receive :: Behavior m a

spawn_ :: (Actor a m) => a -> IO (Pid m)
spawn_ = spawn receive

spawnRemote_ :: (Actor a m) => String -> a -> IO (Pid m)
spawnRemote_ ep = spawnRemote ep receive

forwarding :: TChan (Pid m, m) -> Pid m -> Behavior m a -> a -> IO (Async a)
forwarding mailbox self receive state = async $ forever $ logException $ do
    (sender, msg) <- atomically (readTChan mailbox)
    receive (Context self sender) state msg
