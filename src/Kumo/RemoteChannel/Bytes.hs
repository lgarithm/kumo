module Kumo.RemoteChannel.Bytes where
import           Data.Bits (shiftL, shiftR, (.|.))
import           Data.Word (Word16, Word32, Word8)

pack16 :: Word8 -> Word8 -> Word16
pack16 a b = (((fromIntegral . fromEnum $ a) :: Word16) `shiftL` 8) .|.
                (((fromIntegral . fromEnum $ b) :: Word16))

pack32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
pack32 a b c d = (((fromIntegral . fromEnum $ a) :: Word32) `shiftL` 24) .|.
                    (((fromIntegral . fromEnum $ b) :: Word32) `shiftL` 16) .|.
                    (((fromIntegral . fromEnum $ c) :: Word32) `shiftL` 8) .|.
                    (((fromIntegral . fromEnum $ d) :: Word32))

unpack16 :: Word16 -> (Word8, Word8)
unpack16 w = ( fromIntegral . fromEnum $ w `shiftR` 8
                , fromIntegral . fromEnum $ w
                )

unpack32 :: Word32 -> (Word8, Word8, Word8, Word8)
unpack32 w = ( fromIntegral . fromEnum $ w `shiftR` 24
                , fromIntegral . fromEnum $ w `shiftR` 16
                , fromIntegral . fromEnum $ w `shiftR` 8
                , fromIntegral . fromEnum $ w
                )
