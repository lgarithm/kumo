import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Concurrent.MVar  (MVar, newMVar, putMVar, tryTakeMVar)
import           Control.Monad            (void)
import           Kumo.Actor               (Behavior, Context (..), Pid, host,
                                           remote, send, spawnRemote, tell)
import           Model                    (Msg (..), Result (Result), Task (..))
import           Text.Printf              (printf)
import           Utils                    (runWebApp)

data Worker = Worker { master :: Pid Msg
                     , mutex  :: MVar ()
                     }

work :: Task -> IO Result
work task = do
    sleep 3
    let tid = _id task
    print tid
    return $ Result tid ""

tryWithMVar :: MVar a -> b -> (a -> IO b) -> IO b
tryWithMVar mv zero f = do
    mx <- tryTakeMVar mv
    case mx of
        Nothing -> do
            print "give up"
            return zero
        Just x  -> do
            y <- f x
            putMVar mv x
            return y

receive :: Behavior Msg Worker
receive (Context self sender) worker msg =
    case msg of
        Ready -> do
            send self (master worker) Apply
            print "applied"
        Assign task -> void $ async $ do
            print "task assigned"
            -- result <- withMVar (mutex worker) $ const (work task)
            mResult <- tryWithMVar (mutex worker) Nothing $ const (fmap Just (work task))
            case mResult of
                Just result -> do
                    print "task done"
                    send self sender (Submit result)
                    print "result submitted"
                    tell self Ready
                Nothing -> return ()
        _           -> return ()


main = do
    let port = 3001
    let master = "http://127.0.0.1:3000/"
    let ep = printf "http://127.0.0.1:%d/" port
    mutex <- newMVar ()
    let worker = Worker (remote master) mutex
    pid <- spawnRemote ep receive worker
    th <- async $ runWebApp port (host pid)
    tell pid Ready
    wait th

sleep n = threadDelay $ n * 1000000
