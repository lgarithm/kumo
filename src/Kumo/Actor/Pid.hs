{-# LANGUAGE OverloadedStrings #-}

module Kumo.Actor.Pid
    ( Pid(Nobody)
    , local
    , remote
    , send
    , tell
    , hdrSender
    , Serializable(deserialize, serialize)
    ) where

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, writeTChan)
import           Control.Monad                (void)
import           Data.ByteString              (ByteString, length)
import           Kumo.Actor.Exception         (logException)
import           Network.HTTP
    ( HeaderName (HdrContentLength, HdrContentType, HdrCustom)
    , Request (Request)
    , RequestMethod (POST)
    , mkHeader
    , simpleHTTP
    )
import           Network.URI                  (parseURI)
import           Prelude                      hiding (length)


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

class Serializable m where
  serialize :: m -> ByteString
  deserialize :: ByteString -> Maybe m

sendRemote :: (Serializable m) => Pid m -> String -> m -> IO ()
sendRemote sender url msg =
  case parseURI url of
    Nothing -> print $ "Not a valid URL - " ++ url
    Just u  -> let body = serialize msg
                   headers = [ mkHeader (HdrCustom hdrSender) (show sender)
                             , mkHeader HdrContentType "application/octet-stream"
                             , mkHeader HdrContentLength (show $ length body)
                             ]
               in  void $ simpleHTTP $ Request u POST headers body

send :: (Serializable m) => Pid m -> Pid m -> m -> IO ()
send sender pid msg =
    case pid of
        Nobody        -> return ()
        Local ch      -> atomically $ writeTChan ch (sender, msg)
        Remote _ _ ep -> logException (sendRemote sender ep msg)

tell :: (Serializable a) => Pid a -> a -> IO ()
tell = send {-- from --} Nobody
