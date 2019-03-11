{-# LANGUAGE OverloadedStrings #-}


import           Control.Concurrent.Async (async, wait)
import           Kumo.RemoteChannel       (expose, sendTo)
import           Kumo.RemoteChannel.Peer  (Peer (Peer))

localhost = 0x7f000001
port = 8080

p = Peer localhost port

main = do
    th <- async $ expose p
    sendTo p "1"
    sendTo p "22"
    sendTo p "333"
    sendTo p "4444"
    sendTo p "55555"
    wait th
