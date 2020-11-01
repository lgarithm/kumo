module Kumo.RemoteChannel.Peer where
import           Data.ByteString          as BS (ByteString, pack, unpack)
import           Data.Word                (Word16, Word32)
import           Kumo.RemoteChannel.Bytes

data Peer = Peer { hostIPv4 :: Word32
                 , hostPort :: Word16
                 }

encode :: Peer -> ByteString
encode (Peer ipv4 port) =
    let (a, b, c, d) = unpack32 ipv4
        (e, f) = unpack16 port
    in  BS.pack [a, b, c, d, e, f]

decode :: ByteString -> Peer
decode bs =
    let [ a,b,c,d,e,f ] = BS.unpack bs
    in  Peer (pack32 a b c d) (pack16 e f)

showIPv4 :: Word32 -> String
showIPv4 ip = let (a,b,c,d) = unpack32 ip
              in  (show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d)

instance Show Peer where
    show (Peer ipv4 port) = showIPv4 ipv4 ++ ":" ++ show port
