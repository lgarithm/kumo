{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Kumo.Actor.Remote where


import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.CaseInsensitive  as CI (mk)
import           Data.List.Split       (splitOn)
import           Kumo.Actor.Pid        (Pid (Nobody), hdrSender, remote, send)
import           Network.HTTP.Types    (status200)
import           Network.Wai           (Request, Response, rawPathInfo,
                                        requestBody, requestHeaders,
                                        requestMethod, responseLBS)

type Responder a = (Response -> IO a) -> IO a
type Handler a = Request -> Responder a

host :: forall m b . (Read m, Show m) => Pid m -> Handler b
host pid req respond =
    let path = C8.unpack $ rawPathInfo req :: String
        parts = filter (not . null) $ splitOn "/" path
        method = requestMethod req
        sender = parseSender req
    in  case (method, parts) of
        ("POST", _) -> do
            body <- requestBody req
            let text = C8.unpack body :: String
            let msg = read text :: m
            send sender pid msg
            respond $ okWithText "received"
        _           -> respond $ okWithText "OK"

parseSender req =
    let hdrs = requestHeaders req
    in  case lookup (CI.mk . C8.pack $ hdrSender) hdrs of
        Nothing       -> Nobody
        Just str -> case words (C8.unpack str) of
                        ["remote", endpoint] -> remote endpoint
                        _                    -> Nobody

okWithText = responseLBS status200 [(CI.mk "Content-Type", "text/plain")]
