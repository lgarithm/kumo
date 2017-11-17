import           Master             (runMaster)
import           System.Environment (getArgs)
import           Worker             (runWorker)

main = do
    args <- getArgs
    print args
    case args of
        ["master"] -> runMaster
        ["worker"] -> runWorker
        _          -> return ()
