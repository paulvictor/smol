module Data.Bitmap(Bitmap, emptyBM, setBitBM, checkBit, popCountToRightOf) where

import Data.Word
import Data.Serialize
import Data.Ser
import Control.Lens.Combinators
import Control.Lens.Operators
import Numeric.Lens
import Data.Bits
-- bitmap is a vector of 64 bits
newtype Bitmap = Bitmap { _unBitmap :: Word64 } deriving (Eq)

instance Show Bitmap where
  show (Bitmap bm) =
    "Ox" <> (bm ^. re hex)

instance Serialize Bitmap where
  {-# INLINE put #-}
  put (Bitmap bm) = putWord64be bm
  get = Bitmap <$> getWord64be

instance Ser Bitmap where
  ser (Bitmap bm) = ser bm

{-# INLINE emptyBM #-}
emptyBM :: Bitmap
emptyBM = Bitmap zeroBits

{-# INLINE setBitBM #-}
setBitBM :: Word8 -> Bitmap -> Bitmap
setBitBM i (Bitmap bm) = Bitmap $ setBit bm (fromEnum i)

{-# INLINE checkBit #-}
checkBit :: Bitmap -> Word8 -> Bool
checkBit (Bitmap bm) i = testBit bm (fromEnum i)

{-# INLINE popCountToRightOf #-}
popCountToRightOf :: Word8 -> Bitmap -> Int
popCountToRightOf pos (Bitmap bm) =
  let
    bitMask = (1 `shiftL` fromEnum pos) - 1
  in
    popCount $ bitMask .&. bm

