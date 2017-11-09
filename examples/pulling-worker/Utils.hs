module Utils where

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (getArgs)
import           Text.Printf              (printf)

runWebApp :: Int -> Application -> IO ()
runWebApp defaultPort app = fmap parsePort getArgs >>=
       logInfof (printf "http://localhost:%d/") >>=
       flip run app
  where parsePort [x] = read x :: Int
        parsePort _   = defaultPort

logInfof :: Show a => (a -> String) -> a -> IO a
logInfof fmt x = putStrLn ("[INFO] " ++ fmt x) >> return x
