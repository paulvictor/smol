{-# LANGUAGE DataKinds #-}
module Data.Get where

import Data.Hashable
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Data.VarLength
import Data.Bits
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (MonadState)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import Data.Word
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Tagged

type Pos = Int

type a @ b = Tagged b a

data GetError
  = OutOfBound Int

data Endian = BigE | LitE

type MonadGet m = (MonadReader ByteString m, MonadState Pos m, MonadError GetError m)

class Get a where
  get :: MonadGet m => m a

instance Get Word8 where
  get = do
    bs <- ask
    pos <- State.get
    let
      (fp, l) = BS.toForeignPtr0 bs
    when (pos > l) $
      throwError (OutOfBound pos)
    let
      v =
        unsafePerformIO $
          withForeignPtr fp (\ptr -> peekByteOff ptr pos)
    State.modify (+1)
    return $! v

instance Get (Word16 @ BigE) where
  get = do
    bs <- ask
    pos <- State.get
    let
      (fp, l) = BS.toForeignPtr0 bs
    when (pos+1 > l) $
      throwError (OutOfBound pos)
    let
      v =
        unsafePerformIO $
          withForeignPtr fp (\ptr -> do
            (msb :: Word8) <- peekByteOff ptr pos
            (lsb :: Word8) <- peekByteOff ptr (pos+1)
            return $! fromIntegral $ (msb .<<. 8) .|. lsb)
    State.modify (+2)
    return $! pure v

instance Get (Word16 @ LitE) where
  get = do
    bs <- ask
    pos <- State.get
    let
      (fp, l) = BS.toForeignPtr0 bs
    when (pos+1 > l) $
      throwError (OutOfBound pos)
    let
      v =
        unsafePerformIO $
          withForeignPtr fp (\ptr -> do
            (lsb :: Word8) <- peekByteOff ptr pos
            (msb :: Word8) <- peekByteOff ptr (pos+1)
            return $! fromIntegral $ (msb .<<. 8) .|. lsb)
    State.modify (+2)
    return $! pure v

instance Get (Word32 @ BigE) where
  get = do
    bs <- ask
    pos <- State.get
    let
      (fp, l) = BS.toForeignPtr0 bs
    when (pos+3 > l) $
      throwError (OutOfBound pos)
    let
      v =
        unsafePerformIO $
          withForeignPtr fp (\ptr -> do
            (msb :: Word8) <- peekByteOff ptr pos
            (msb2 :: Word8) <- peekByteOff ptr pos
            (msb3 :: Word8) <- peekByteOff ptr pos
            (lsb :: Word8) <- peekByteOff ptr (pos+1)
            return $! fromIntegral $ (msb .<<. 24) .|. (msb2 .<<. 16) .|. (msb3 .<<. 8) .|. lsb)
    State.modify (+4)
    return $! pure v

instance (Get VarLength) where
  get =
    _VarLength <$> loop 0
    where
    loop prev = do
      w8 <- get @Word8
      let
        !next = (prev .<<. 7) .|. (fromIntegral (clearBit w8 7))
      if testBit w8 7
        then loop next -- we need to get more bytes
        else -- at the last byte. The msb should be 0
          return $ (prev .<<. 7) .|. next -- could have used next?

instance Get ByteString where
  get = do
    len <- fromIntegral . _unVarLength <$> get @VarLength
    pos <- State.get
    bs <- ask
    when (pos + len > BS.length bs) $
      throwError (OutOfBound pos)
    return $!
      BS.take len $ BS.drop pos bs

instance Get a => Get [a] where
  get = do
    len <- fromIntegral . _unVarLength <$> get @VarLength
    replicateM len get

instance (Get a, Get b) => Get (a, b) where
  get = (,) <$> get <*> get

instance (Get a, Ord a, Get b) => Get (Map a b) where
  get = do
    len <- _unVarLength <$> get @VarLength
    loop len mempty
    where
    loop 0 m = return m
    loop i !m = do
      (!k, !v) <- get
      loop (i-1) (Map.insert k v m)

instance (Get a, Hashable a, Get b) => Get (HashMap a b) where
  get = do
    len <- _unVarLength <$> get @VarLength
    loop len mempty
    where
    loop 0 m = return m
    loop i !m = do
      (!k, !v) <- get
      loop (i-1) (HashMap.insert k v m)

lookAhead :: (Get a, MonadGet m) => m a
lookAhead = do
  pos <- State.get
  get <* State.put pos

position :: (MonadGet m) => m Int
position = State.get
