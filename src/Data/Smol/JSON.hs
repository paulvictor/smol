{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Smol.JSON where

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Aeson.KeyMap as KM
import Data.Serialize
import qualified Data.Vector as V'
import qualified Data.Text.Encoding as T
import Control.Lens.Operators
import Control.Lens.Combinators
import Data.VarLength
import Data.Scientific

-- We use this because the default put encodes the length as Int64 which may not be necessary
instance Serialize Key where
  {-# INLINE put #-}
  put k = putLengthEncodedBS (k ^. from _Key)
  {-# INLINE get #-}
  get = view _Key <$> getLengthEncodedBS

instance Serialize Value where
  {-# INLINE put #-}
  put = \case
    Object !o -> do
      putWord8 0
      putMapOf put put (KM.toMap o)
    Array !v -> do
      putWord8 1
      put (_VarLength . fromIntegral $ (V'.length v))
      mapM_ put v
    Number !n ->
      either
        (\d -> putWord8 2 >> put @Double d)
        (\i -> putWord8 3 >> putInt64be i)
        (floatingOrInteger n)
    String !t -> do
      putWord8 4
      putLengthEncodedBS $ T.encodeUtf8 t
    Bool !b -> do
      putWord8 5
      put b
    Null ->
      putWord8 6

  {-# INLINE get #-}
  get = getWord8 >>= \case
    0 -> Object . KM.fromMap <$> getMapOf get get -- TODO, can we use same serialization
    1 -> do
      l <- fromEnum . _unVarLength <$> get @VarLength
      Array <$> V'.replicateM l get
    2 -> Number . fromFloatDigits <$> get @Double
    3 -> Number . fromIntegral <$> getInt64be
    4 -> String . T.decodeUtf8 <$> getLengthEncodedBS
    5 -> Bool <$> get
    6 -> return Null
    _ -> fail "Expected w8 less than 7"
