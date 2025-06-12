module Data.VarLength
  ( VarLength
  , varLengthEncoded
  , getLengthEncoded
  , _VarLength
  , getVarLength
  , putLengthEncodedBS
  , getLengthEncodedBS
  , _unVarLength ) where

import Data.Serialize
import Data.Bits
import Data.Functor
import Data.ByteString (ByteString)
-- The bool represents if we need to set the msb
-- If unknown, pass False.
-- Should we create a type alias for external use?
newtype VarLength' (k :: Bool) = VarLength { _unVarLength :: Word } deriving (Eq,Show)

_VarLength :: Word -> VarLength
_VarLength = VarLength

getVarLength :: Get (VarLength' b)
getVarLength = VarLength <$> go (0 :: Word)
  where
  go prev = do
    w8 <- getWord8
    let
      next = (prev .<<. 7) .|. (fromIntegral (clearBit w8 7))
    if testBit w8 7
      then go next -- we need to get more bytes
      else -- at the last byte. The msb should be 0
        return $ (prev .<<. 7) .|. fromIntegral w8 -- could have used next?

instance Serialize (VarLength' False) where
  put (VarLength i) =
    let
      quot128 = i `shiftR` 7
      rem128 = i .&. ((bit 7) - 1)
    in if i < 128 then putWord8 (fromIntegral i)
    else put (VarLength @True quot128) >> putWord8 (fromIntegral rem128)
  get = getVarLength

instance Serialize (VarLength' True) where
  put (VarLength i) =
    let
      quot128 = i `shiftR` 7
      rem128 = (i .&. ((bit 7) - 1)) .|. bit 7
    in
      if i == 0
      then return ()
      else put (VarLength @True quot128) >> putWord8 (fromIntegral rem128)
  get = getVarLength

type VarLength = VarLength' False

varLengthEncoded :: Serialize a => Putter a
varLengthEncoded a =
  putNested
    (put . _VarLength . fromIntegral) -- Encoded data structures cannot be > 4GB in length
    (put a)

getLengthEncoded :: Serialize a => Get a
getLengthEncoded =
  getNested (getVarLength <&> fromEnum . _unVarLength) get

putLengthEncodedBS :: Putter ByteString
putLengthEncodedBS bs =
  putNested
    (put . _VarLength . fromIntegral)
    (putByteString bs)

getLengthEncodedBS :: Get ByteString
getLengthEncodedBS =
  get @VarLength >>= getBytes . fromIntegral . _unVarLength

