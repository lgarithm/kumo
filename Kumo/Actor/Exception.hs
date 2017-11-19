module Kumo.Actor.Exception where

import           Control.Exception (SomeException, catch)

pErr :: SomeException -> IO ()
pErr e = let err = show (e :: SomeException)
         in  putStrLn $ "exception: " ++ err

logException :: IO () -> IO ()
logException f = catch f pErr
