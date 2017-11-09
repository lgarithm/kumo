module Model where

import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import           Kumo.Actor            (Serializable (deserialize, serialize))

data Result = Result { taskID :: String
                     , status :: String
                     }
            deriving (Show, Read)

data Task = Task { _id  :: String
                 , name :: String
                 }
          deriving (Show, Read)

data Msg = Apply
         | Assign Task
         | Submit Result
         | Ready
         deriving (Show, Read)

instance Serializable Msg where
    serialize = C8.pack . show
    deserialize = Just . read . C8.unpack
