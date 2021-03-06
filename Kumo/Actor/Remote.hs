{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kumo.Actor.Remote where


import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.ByteString.Lazy  as LBS (toStrict)
import qualified Data.CaseInsensitive  as CI (mk)
import           Data.List.Split       (splitOn)
import           Data.Maybe            (fromMaybe)
import           Kumo.Actor.Pid
    ( Pid (Nobody)
    , Serializable (deserialize)
    , hdrSender
    , remote
    , send
    )
import           Network.HTTP.Types    (status200)
import           Network.URI           (parseURI)
import           Network.Wai
    ( Request
    , Response
    , rawPathInfo
    , requestHeaders
    , requestMethod
    , responseLBS
    , strictRequestBody
    )

type Responder a = (Response -> IO a) -> IO a
type Handler a = Request -> Responder a

host :: forall m b . (Serializable m) => Pid m -> Handler b
host pid req respond =
    let path = C8.unpack $ rawPathInfo req :: String
        parts = filter (not . null) $ splitOn "/" path
        method = requestMethod req
        sender = parseSender req
    in  case (method, parts) of
        ("POST", _) -> do
            body <- fmap LBS.toStrict (strictRequestBody req)
            let result = deserialize body :: Maybe m
            case result of
                Just msg -> send sender pid msg >> respond (okWithText "received")
                _        -> print "deserialize failed" >> respond (okWithText "deserialize failed")
        _           -> respond $ okWithText "OK"

parseSender :: Request -> Pid m
parseSender req = fromMaybe Nobody $
    lookup (CI.mk . C8.pack $ hdrSender) (requestHeaders req) >>=
    parseRemote                                               >>=
    parseURI                                                  >>=
    Just . remote . show
        where parseRemote str = case words (C8.unpack str) of
                ["remote", endpoint] -> Just endpoint
                _                    -> Nothing

okWithText = responseLBS status200 [(CI.mk "Content-Type", "text/plain")]
