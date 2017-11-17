module Utils where

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           System.IO                (hFlush, stdout)
import           Text.Printf              (printf)

runWebApp :: Int -> Application -> IO ()
runWebApp port app = logInfof (printf "http://localhost:%d/") port >>= flip run app

logInfof :: Show a => (a -> String) -> a -> IO a
logInfof fmt x = do
      putStrLn ("[INFO] " ++ fmt x)
      hFlush stdout
      return x
