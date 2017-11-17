module Master where

import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad           (replicateM_)
import           Kumo.Actor
    ( Behavior
    , Context (..)
    , host
    , send
    , spawnRemote
    )
import           Model                   (Msg (..), Result (..), Task (..))
import           Utils                   (runWebApp)

data Master = Master { _id     :: String
                     , counter :: MVar Int
                     }

updateMvar :: MVar a -> (a -> a) -> IO a
updateMvar mv f = do
    x <- takeMVar mv
    let y = f x
    putMVar mv y
    return y

allocate :: Master -> IO (Maybe Task)
allocate master = do
    n <- updateMvar (counter master) (+1)
    print $ "allocate " ++ show n
    return $ Just $ Task (show n) ("task-" ++ show n)

receive :: Behavior Msg Master
receive (Context self sender) master msg =
    case msg of
    Apply         -> do
        print $ show sender ++ " applied"
        replicateM_ 3 $ do
            mTask <- allocate master
            case mTask of
                Just task -> send self sender (Assign task)
                Nothing   -> return ()
    Submit result -> print $ "submitted " ++ taskID result
    _             -> return ()


runMaster = do
    let ep = "http://master:3000/"
    print ep
    counter <- newMVar 0
    let master = Master "master0" counter
    pid <- spawnRemote ep receive master
    runWebApp 3000 (host pid)
