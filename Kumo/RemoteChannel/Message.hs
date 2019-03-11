{-# LANGUAGE OverloadedStrings #-}

module Kumo.RemoteChannel.Message where
import           Data.ByteString          as BS
    ( ByteString
    , length
    , pack
    , unpack
    )
import           Data.Word                (Word32)
import           Kumo.RemoteChannel.Bytes

data MessageHeader =  MessageHeader { msgSize :: Word32 } deriving (Show)


mkHeader :: ByteString -> MessageHeader
mkHeader bs = MessageHeader (fromIntegral $ BS.length bs)

encode :: MessageHeader -> ByteString
encode (MessageHeader size) =
    let (a, b, c, d) = unpack32 size
    in  BS.pack [a, b, c, d]

decode :: ByteString -> MessageHeader
decode bs =
    let [ a,b,c,d ] = BS.unpack bs
    in  MessageHeader (pack32 a b c d)
