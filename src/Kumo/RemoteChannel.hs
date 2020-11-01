{-# LANGUAGE OverloadedStrings #-}


module Kumo.RemoteChannel where

import           Data.ByteString            as BS (ByteString)
import           Kumo.RemoteChannel.Message as M
import           Kumo.RemoteChannel.Peer    as P
import           Network.Simple.TCP
    ( HostPreference (Host)
    , connect
    , recv
    , send
    , serve
    )

logInfof :: Show a => String -> (a -> String) -> a -> IO a
logInfof prefix fmt x = putStrLn (prefix ++ fmt x) >> return x

sendTo :: Peer -> ByteString -> IO ()
sendTo peer@(Peer ipv4 port) bs =
    connect (showIPv4 ipv4) (show port) $ handle where
        handle (sock, remoteAddr) = do
            logInfof "[send] " show sock
            logInfof "[send] " show remoteAddr
            send sock $ P.encode peer
            logInfof "[send] " show peer
            let hdr = mkHeader bs
            send sock $ M.encode hdr
            send sock $ bs
            return ()

expose :: Peer -> IO ()
expose (Peer ipv4 port) = do
    serve (Host $ showIPv4 ipv4) (show port) $ handle where
        handle (sock, remoteAddr) = do
            logInfof "[recv] " show sock
            logInfof "[recv] " show remoteAddr
            Just bs <- recv sock 6
            let peer = P.decode bs
            logInfof "[recv] " show peer
            Just bs <- recv sock 4
            let hdr = M.decode bs
            logInfof "[recv] " show hdr
            return ()
