module Data.VarLength
  ( VarLength(..)
  , varLengthEncoded
  , getLengthEncoded
  , getVarLength
  , putLengthEncodedBS
  , getLengthEncodedBS) where

import Data.Word
import Data.Serialize
import Data.Bits
import Data.Functor
import Data.ByteString (ByteString)

-- The bool represents if we need to set the msb
-- If unknown, pass False.
-- Should we create a type alias for external use?
newtype VarLength = VarLength { _unVarLength :: Word } deriving (Eq,Show)

{-# INLINE getVarLength #-}
getVarLength :: Get (VarLength)
getVarLength = VarLength <$> go (0 :: Word)
  where
  go !prev = do
    w8 <- getWord8
    let
      !next = (prev .<<. 7) .|. (fromIntegral (clearBit w8 7))
    if testBit w8 7
      then go next -- we need to get more bytes
      else -- at the last byte. The msb should be 0
        return $ (prev .<<. 7) .|. next -- could have used next?

{-# INLINE last7BitsSet #-}
last7BitsSet :: Word8
last7BitsSet = (bit 7) - 1

instance Serialize VarLength where
  put (VarLength i) =
    loop i False
    where
    loop :: Word -> Bool -> Put
    loop !j b =
      let
        quot128 = j .>>. 7
        !rem128 :: Word8 = fromIntegral j .&. last7BitsSet
        serializing = putWord8 (if b then rem128 .|. bit 7 else rem128)
      in
        if quot128 == 0
        then serializing
        else loop quot128 True *> serializing
  get = getVarLength

{-# INLINE varLengthEncoded #-}
varLengthEncoded :: Serialize a => Putter a
varLengthEncoded a =
  putNested
    (put . VarLength . fromIntegral) -- Encoded data structures cannot be > 4GB in length
    (put a)

{-# INLINE getLengthEncoded #-}
getLengthEncoded :: Serialize a => Get a
getLengthEncoded =
  getNested (getVarLength <&> fromEnum . _unVarLength) get

{-# INLINE putLengthEncodedBS #-}
putLengthEncodedBS :: Putter ByteString
putLengthEncodedBS bs =
  putNested
    (put . VarLength . fromIntegral)
    (putByteString bs)

{-# INLINE getLengthEncodedBS #-}
getLengthEncodedBS :: Get ByteString
getLengthEncodedBS =
  get @VarLength >>= getBytes . fromIntegral . _unVarLength
