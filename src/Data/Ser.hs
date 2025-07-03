module Data.Ser where

import Data.Scientific (floatingOrInteger)
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Map.Strict ( Map)
import qualified Data.HashMap.Strict as HM
import  Data.HashMap.Strict (HashMap)
import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Key)
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Data.ByteString (ByteString)
import Control.Monad
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Semigroup

data BufferWithLen = BufferWithLen
  { _builder :: !Builder
  -- The length is needed because builders do not provide it out of the box and calculating it is expensive
  , _length :: !(Sum Int)
  }

instance Semigroup BufferWithLen where
  {-# INLINE (<>) #-}
  (BufferWithLen b1 l1) <> BufferWithLen b2 l2 = BufferWithLen (b1 <> b2) (l1 <> l2)

instance Monoid BufferWithLen where
  mempty = BufferWithLen mempty mempty

newtype SerializingM a = -- Equivalent to a Proxy a with a monoidal buffer
  Serializing {runSerializing :: BufferWithLen} deriving (Functor)

instance Semigroup (SerializingM ()) where
  {-# INLINE (<>) #-}
  (Serializing b1) <> Serializing b2 = Serializing (b1 <> b2)

instance Monoid (SerializingM ()) where
  {-# INLINE mempty #-}
  mempty = Serializing mempty

instance Applicative SerializingM where
  {-# INLINE pure #-}
  pure _ = Serializing mempty
  (<*>) = ap
  Serializing buf1 *> Serializing buf2 = Serializing (buf1 <> buf2)

instance Monad SerializingM where
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  Serializing serBuf0 >>= f =
    let
      Serializing serBuf1 = f undefined
    in Serializing (serBuf0 <> serBuf1)

  {-# INLINE (>>) #-}
  (>>) = (*>)

type Serializing = SerializingM ()

class Ser a where
  ser :: a -> Serializing

{-# INLINE tell #-}
tell :: BufferWithLen -> Serializing
tell sb = Serializing sb

instance Ser Bool where
  {-# INLINE ser #-}
  ser True = word8 0
  ser False = word8 1

instance Ser Word8 where
  {-# INLINE ser #-}
  ser = word8

{-# INLINE word8 #-}
word8 :: Word8 -> Serializing
word8 w8 = tell (BufferWithLen (Builder.word8 w8) (Sum 1))

instance Ser Word16 where
  {-# INLINE ser #-}
  ser = word16BE

{-# INLINE word16BE #-}
word16BE :: Word16 -> Serializing
word16BE w16 = tell (BufferWithLen (Builder.word16BE w16) (Sum 2))

instance Ser Word32 where
  {-# INLINE ser #-}
  ser = word32BE

{-# INLINE word32BE #-}
word32BE :: Word32 -> Serializing
word32BE w32 = tell (BufferWithLen (Builder.word32BE w32) (Sum 4))

instance Ser Word64 where
  {-# INLINE ser #-}
  ser = word64BE

{-# INLINE word64BE #-}
word64BE :: Word64 -> Serializing
word64BE w64 = tell (BufferWithLen (Builder.word64BE w64) (Sum 4))

instance Ser Double where
  {-# INLINE ser #-}
  ser d = tell $ BufferWithLen (Builder.doubleBE d) (Sum 8)

instance Ser Int where
  {-# INLINE ser #-}
  ser i = tell $ BufferWithLen (Builder.int64BE (toEnum i)) (Sum 8)

{-# INLINABLE varLength #-}
varLength :: Int -> Serializing
varLength i =
  if i < 128 then word8 (fromIntegral i)
  else loop i
  where
  loop !j =
    let
      quot128 = j .<<. 7
      rem128 = j .&. ((bit 7) - 1)
    in
      if j == 0
      then return ()
      else loop quot128 *> word8 (fromIntegral  rem128)

instance Ser a => Ser (Sum a) where
  {-# INLINE ser #-}
  ser (Sum a) = ser a

instance Ser ByteString where
  {-# INLINE ser #-}
  ser = byteString

{-# INLINE byteString #-}
byteString :: ByteString -> Serializing
byteString bs =
  let
    l = BS.length bs
    serBuf = BufferWithLen (Builder.byteString bs) (Sum l)
  in varLength l >> tell serBuf

builder :: Builder -> Int -> Serializing
builder b l = Serializing $ BufferWithLen b (Sum l)

instance Ser Key where
  {-# INLINE ser #-}
  ser k = byteString (k ^. from _Key)

instance Ser a => Ser [a] where
  {-# INLINE ser #-}
  ser = listOf

{-# INLINE listOf #-}
listOf :: Ser a => [a] -> Serializing
listOf xs =
  let
    !l = length xs
  in
    varLength l >> foldMap ser xs

nested :: (Int -> Serializing) -> Serializing -> Serializing
nested serInt s =
  let
    (BufferWithLen bldr1 l1) = runSerializing s
    (BufferWithLen bldr2 l2) = runSerializing (serInt (getSum l1))
  in
    tell $ BufferWithLen (bldr2 <> bldr1) (l1 <> l2)

instance Ser a => Ser (Vector a) where
  {-# INLINE ser #-}
  ser xs =
   let
     l = V.length xs
   in
    varLength l >> V.foldMap' ser xs

instance (Ser k, Ser v) => Ser (Map k v) where
  {-# INLINE ser #-}
  ser m =
    let
      l = Map.size m
    in do
      varLength l
      Map.foldMapWithKey (\k v -> ser k >> ser v) m

instance (Ser k, Ser v) => Ser (HashMap k v) where
  {-# INLINE ser #-}
  ser m =
    let
      l = HM.size m
    in do
      varLength l
      HM.foldMapWithKey (\k v -> ser k >> ser v) m

instance (Ser v) => Ser (KeyMap v) where
  {-# INLINE ser #-}
  ser km = ser (KM.toHashMap km)

instance Ser Value where
  {-# INLINE ser #-}
  ser = \case
    Object !o -> word8 0 >> ser o
    Array !v -> word8 1 >> ser v
    Number !n ->
      either
        (\d -> word8 2 >> ser @Double d)
        (\i -> word8 3 >> (ser @Int) i)
        (floatingOrInteger n)
    String !t -> word8 4 >> (ser $ T.encodeUtf8 t)
    Bool !b -> word8 5 >> ser b
    Null -> word8 6
