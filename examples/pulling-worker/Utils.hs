module Utils where

import           Data.Time.Clock          (getCurrentTime)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           System.IO                (hFlush, stdout)
import           Text.Printf              (printf)


runWebApp :: Int -> Application -> IO ()
runWebApp port app = logInfof (printf "http://localhost:%d/") port >>= flip run app

logInfof :: Show a => (a -> String) -> a -> IO a
logInfof fmt x = do
      t <- getCurrentTime
      putStrLn (show t ++ " [I] " ++ fmt x)
      -- flush
      return x

logRaw :: Show a => a -> IO ()
logRaw msg = do
      t <- getCurrentTime
      -- flush
      putStrLn $ show t ++ " [I] " ++ show msg

flush = hFlush stdout
