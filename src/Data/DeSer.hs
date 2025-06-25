{-# LANGUAGE DataKinds #-}
module Data.DeSer
  ( runDeSer
  , DeSerError
  , DeSer
  , MonadDeSer
  , Endian(..)
  , lookAhead
  , bytesConsumed
  , deserTwo
  , deserMany
  , type (@)
  , deserManyTillEnd
  , skip
  , deser) where

import Control.Lens.Combinators
import Data.Aeson.Lens
import Data.Scientific (fromFloatDigits)
import Foreign.Ptr (castPtr, Ptr)
import Data.Int
import qualified Data.Text.Encoding as T
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Data.VarLength
import Data.Bits
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff, peek, poke)
import Data.Word
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import Foreign.Marshal.Alloc (alloca)
import Control.Monad
import Data.ByteString.Unsafe
import Data.Tagged

type Pos = Int

type a @ b = Tagged b a

data DeSerError
  = OutOfBound Int
  | InvalidEncoding String deriving (Show, Eq)

data Endian = BigE | LitE

type MonadDeSer m = (MonadReader ByteString m, MonadState Pos m, MonadError DeSerError m)

type DeSerializing = StateT Pos (ReaderT ByteString (Either DeSerError))
class DeSer a where
  deser :: MonadDeSer m => m a

{-# INLINE ensure #-}
ensure :: MonadDeSer m => Int -> m ()
ensure i = do
  l <- ask <&> BS.length
  pos <- State.get
  when (pos + i > l) $
    throwError (OutOfBound pos)

{-# INLINE getBytes #-}
getBytes :: MonadDeSer m => Int -> m ByteString
getBytes i = do
  ensure i
  pos <- State.get
  bs <- ask
  State.modify (+i)
  return $ BS.take i $ BS.drop pos bs

instance DeSer Word8 where
  {-# INLINE deser #-}
  deser = getBytes 1 <&> unsafeHead

instance DeSer (Word16 @ BigE) where
  {-# INLINE deser #-}
  deser = do
    bs <- getBytes 2
    let
      (fp, _) = BS.toForeignPtr0 bs
    return $
      unsafePerformIO $
        withForeignPtr fp (\ptr -> do
          (msb :: Word8) <- peekByteOff ptr 0
          (lsb :: Word8) <- peekByteOff ptr 1
          return $! fromIntegral $ (msb .<<. 8) .|. lsb)

instance DeSer (Word16 @ LitE) where
  {-# INLINE deser #-}
  deser = do
    bs <- getBytes 2
    let
      (fp, _) = BS.toForeignPtr0 bs
    return $
      unsafePerformIO $
        withForeignPtr fp (\ptr -> do
          (lsb :: Word8) <- peekByteOff ptr 0
          (msb :: Word8) <- peekByteOff ptr 1
          return $! fromIntegral $ (msb .<<. 8) .|. lsb)

instance DeSer (Word32 @ BigE) where
  {-# INLINE deser #-}
  deser = do
    s <- getBytes 4
    return $! (fromIntegral (s `BS.unsafeIndex` 0) `shiftL` 24) .|.
              (fromIntegral (s `BS.unsafeIndex` 1) `shiftL` 16) .|.
              (fromIntegral (s `BS.unsafeIndex` 2) `shiftL` 8) .|.
              (fromIntegral (s `BS.unsafeIndex` 3))

instance DeSer (Int64 @ BigE) where
  {-# INLINE deser #-}
  deser = do
    s <- getBytes 8
    return $! (fromIntegral (s `BS.unsafeIndex` 0) `shiftL` 56) .|.
              (fromIntegral (s `BS.unsafeIndex` 1) `shiftL` 48) .|.
              (fromIntegral (s `BS.unsafeIndex` 2) `shiftL` 40) .|.
              (fromIntegral (s `BS.unsafeIndex` 3) `shiftL` 32) .|.
              (fromIntegral (s `BS.unsafeIndex` 4) `shiftL` 24) .|.
              (fromIntegral (s `BS.unsafeIndex` 5) `shiftL` 16) .|.
              (fromIntegral (s `BS.unsafeIndex` 6) `shiftL`  8) .|.
              (fromIntegral (s `BS.unsafeIndex` 7))

instance DeSer (Word64 @ BigE) where
  {-# INLINE deser #-}
  deser = do
    s <- getBytes 8
    return $! (fromIntegral (s `BS.unsafeIndex` 0) `shiftL` 56) .|.
              (fromIntegral (s `BS.unsafeIndex` 1) `shiftL` 48) .|.
              (fromIntegral (s `BS.unsafeIndex` 2) `shiftL` 40) .|.
              (fromIntegral (s `BS.unsafeIndex` 3) `shiftL` 32) .|.
              (fromIntegral (s `BS.unsafeIndex` 4) `shiftL` 24) .|.
              (fromIntegral (s `BS.unsafeIndex` 5) `shiftL` 16) .|.
              (fromIntegral (s `BS.unsafeIndex` 6) `shiftL`  8) .|.
              (fromIntegral (s `BS.unsafeIndex` 7))

instance DeSer Double where
  {-# INLINE deser #-}
  deser =
    wordToDouble . unTagged <$> deser @(Word64 @ BigE)
    where
    wordToDouble w = unsafeDupablePerformIO $ alloca $ \(ptr :: Ptr Word64) ->
      poke ptr w >> peek (castPtr ptr)

instance (DeSer VarLength) where
  {-# INLINE deser #-}
  deser =
    _VarLength <$> loop 0
    where
    loop prev = do
      w8 <- deser @Word8
      let
        !next = (prev .<<. 7) .|. (fromIntegral (clearBit w8 7))
      if testBit w8 7
        then loop next -- we need to get more bytes
        else -- at the last byte. The msb should be 0
          return $ (prev .<<. 7) .|. next -- could have used next?

instance DeSer ByteString where
  {-# INLINE deser #-}
  deser =
    deser @VarLength >>= getBytes . fromIntegral . _unVarLength

instance DeSer a => DeSer [a] where
  {-# INLINE deser #-}
  deser = do
    len <- fromIntegral . _unVarLength <$> deser @VarLength
    replicateM len deser

instance (DeSer a, DeSer b) => DeSer (a, b) where
  {-# INLINE deser #-}
  deser = (,) <$> deser <*> deser

instance (DeSer a, Ord a, DeSer b) => DeSer (Map a b) where
  {-# INLINE deser #-}
  deser = do
    len <- _unVarLength <$> deser @VarLength
    loop len mempty
    where
    loop 0 m = return m
    loop i !m = do
      (!k, !v) <- deser
      loop (i-1) (Map.insert k v m)

instance (DeSer a, Hashable a, DeSer b) => DeSer (HashMap a b) where
  {-# INLINE deser #-}
  deser = do
    len <- _unVarLength <$> deser @VarLength
    loop len mempty
    where
    loop 0 m = return m
    loop i !m = do
      (!k, !v) <- deser
      loop (i-1) (HashMap.insert k v m)

instance DeSer Bool where
  {-# INLINE deser #-}
  deser = deser @Word8 >>= \case
    0 -> return False
    1 -> return True
    i -> throwError (InvalidEncoding (show i))

{-# INLINE lookAhead #-}
lookAhead :: (DeSer a, MonadDeSer m) => m a
lookAhead = do
  pos <- State.get
  deser <* State.put pos

bytesConsumed :: (MonadDeSer m) => m Int
bytesConsumed = State.get

{-# INLINE skip #-}
skip :: MonadDeSer m => Int -> m ()
skip i = State.modify (+i)

{-# INLINE atEnd #-}
atEnd :: MonadDeSer m => m Bool
atEnd = do
  maxL <- asks BS.length
  pos <- State.get
  return (pos == maxL - 1)

deserTwo :: DeSerializing k -> DeSerializing v -> DeSerializing (k, v)
deserTwo dk dv = (,) <$> dk <*> dv

{-# INLINE deserManyTillEnd #-}
deserManyTillEnd :: DeSerializing a -> DeSerializing [a]
deserManyTillEnd d = loop []
  where
  loop xs = do
    !a <- d
    reachedEndB <- atEnd
    if reachedEndB
      then return $! xs
      else loop (a:xs)

deserMany :: DeSerializing a -> DeSerializing [a]
deserMany d = do
  len <- fromIntegral . _unVarLength <$> deser @VarLength
  replicateM len d

-- TODO, if we need the unused bytestring, read from pos to end and return it
runDeSer :: DeSerializing a -> ByteString -> Either DeSerError a
runDeSer x =
  runReaderT (evalStateT x 0)

instance DeSer Key where
  {-# INLINE deser #-}
  deser = view _Key <$> deser @ByteString

instance DeSer Value where
  {-# INLINE deser #-}
  deser = deser @Word8 >>= \case
    0 ->
      Object . KM.fromMap <$> deser @(Map Key Value)
    1 -> do
      l <- fromEnum . _unVarLength <$> deser @VarLength
      Array <$> V.replicateM l deser
    2 -> Number . fromFloatDigits <$> deser @Double
    3 -> Number . fromIntegral <$> deser @(Int64 @ BigE)
    4 -> String . T.decodeUtf8 <$> deser @ByteString
    5 -> Bool <$> deser
    6 -> return Null
    i -> throwError $ InvalidEncoding ("Expected w8 less than 7, got " <> show i)
