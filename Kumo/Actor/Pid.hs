{-# LANGUAGE OverloadedStrings #-}

module Kumo.Actor.Pid
    ( Pid(Nobody)
    , local
    , remote
    , send
    , tell
    , hdrSender
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, writeTChan)
import           Control.Exception            (SomeException (..), catch)
import           Network.HTTP                 (insertHeader,
                                               postRequestWithBody, simpleHTTP)
import           Network.HTTP.Headers         (HeaderName (HdrCustom))

-- a Pid is a handle that you can send message to
data Pid m = Remote { _kind     :: String
                    , _name     :: String
                    , _endpoint :: String
                    }
           | Local { _ch :: TChan (Pid m, m) }
           | Nobody

local :: TChan (Pid m, m) -> Pid m
local = Local

remote :: String -> Pid m
remote = Remote "" ""

instance Show (Pid m) where
    show (Remote _ _ endpoint) = "remote " ++ endpoint
    show (Local _)             = "<Local>"
    show Nobody                = "<Nobody>"

hdrSender = "X-Kumo-Actor-Sender"

sendRemote :: (Show m) => Pid m -> String -> m -> IO ()
sendRemote sender ep msg = do
    let req = insertHeader (HdrCustom hdrSender) (show sender) $
              postRequestWithBody ep "text/plain" (show msg)
    simpleHTTP req
    return ()

send :: (Show m) => Pid m -> Pid m -> m -> IO ()
send sender pid msg =
    case pid of
        Nobody        -> return ()
        Local ch      -> atomically $ writeTChan ch (sender, msg)
        Remote _ _ ep -> catch (sendRemote sender ep msg) pErr

tell :: (Show a) => Pid a -> a -> IO ()
tell = send {-- from --} Nobody

pErr :: SomeException -> IO ()
pErr e = let err = show (e :: SomeException)
         in  print $ "exception " ++ err

