{-# LANGUAGE OverloadedStrings #-}

import           Network.HTTP.Types       (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp

type Responder a = (Response -> IO a) -> IO a
type Handler a = Request -> Responder a

handle req respond = do
  respond $ responseLBS status200 [] "OK\n"

main = do
  print "main"
  let settings = defaultSettings
  runSettings settings handle
