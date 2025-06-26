{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Smol
  ( lookupEncodedHamt
  , trimHAMT
  , serializeHAMT
  , lookupHAMT
  , deserializeHAMT
  , LeafElem(..)
  , HAMT(..)
  , Smol(..)
  , fromKVPairs ) where

import Debug.Trace
import Control.Monad.Error.Class (throwError)
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Trans.State.Strict (runState)
import Data.Bitmap
import Data.Bits
import Data.ByteString.Builder (Builder)
import Data.ByteString (ByteString)
import Data.DeSer
import Data.Foldable
import Data.Hashable
import Data.Sequence (Seq(..))
import Data.Serialize
import Data.Traversable
import Data.Tree (drawTree, Tree(..))
import Data.VarLength
import Data.Word
import Foreign.Storable (sizeOf)
import GHC.TypeLits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DeSer as DeSer
import qualified Data.Sequence as Seq

-- Used for construction
data LeafElem k v =
  LeafElem
  {
    _k :: !k
  , _v :: v
  } deriving (Eq, Show, Functor, Foldable, Traversable)
$(makeLenses ''LeafElem)

instance (Serialize k, Serialize v) => Serialize (LeafElem k v) where
  put (LeafElem {_k, _v}) = put _k >> put _v
  {-# INLINE get #-}
  get = LeafElem <$> get <*> get

instance (DeSer k, DeSer v) => DeSer (LeafElem k v) where
  {-# INLINE deser #-}
  deser = LeafElem <$> deser <*> deser

data HAMT k v
  = Internal
    {
      _bitmap :: !Bitmap
    , _orderedChildren :: Seq (HAMT k v)
    }
  | Leaf
    {
      _values :: [LeafElem k v]
    } deriving (Eq, Functor, Foldable, Traversable)

$(makeLenses ''HAMT)
$(makePrisms ''HAMT)

instance (Show k, Show v) => Show (HAMT k v) where
  show = drawTree . fmap show . toTree
    where
    toTree = \case
      Internal bm xs -> Node (Left bm) (toList $ toTree <$> xs)
      Leaf xs -> Node (Right xs) []

trimHAMT :: HAMT k v -> HAMT k v
trimHAMT = \case
  l@(Leaf _) -> l
  node@(Internal {}) ->
    let
      nodeWithTrimmedChildren =
        node &
          orderedChildren.traversed %~ trimHAMT
      firstTrimmed = Seq.index (nodeWithTrimmedChildren ^. orderedChildren) 0
    in
      if Seq.length (node ^. orderedChildren) == 1 && has _Leaf firstTrimmed
      then firstTrimmed
      else nodeWithTrimmedChildren

fromKVPairs :: (Foldable t, Serialize k, Serialize v, Hashable k) => t (k, v) -> HAMT k v
fromKVPairs = foldr (\(k0, v0) hamt -> consHAMT k0 v0 hamt) (empty 5)

empty :: Int -> HAMT k v
empty level =
  if level == 0
  then Leaf []
  else Internal emptyBM []

{-# INLINE lookupHash #-}
lookupHash :: Hashable k => k -> Word64
lookupHash key =
  fromIntegral $ hash key .&. ((1 `shiftL` 36) - 1) -- 0x0000000fffffffff -- We will be using only the last 36 bits anyway

{-# INLINE idxIntoBitmapForPos #-}
idxIntoBitmapForPos :: Int -> Word64 -> Word8
idxIntoBitmapForPos pos h =
  fromIntegral $ (h .>>. (pos*6)) .&. ((1 `shiftL` 6) - 1)

consHAMT :: Hashable k => k -> v -> HAMT k v -> HAMT k v
consHAMT key value = go 5
  where
  h = lookupHash key
  go _ node@(Leaf _) =
    -- This is not really overwriting the key/value but it prepends to the left of the seq and
    -- transfers the responsibility to lookup, which goes in sequence,
    -- so while it is safe wrt to overwriting values, it does not reduce space of previous values
    -- May lead to space leak because the underlying k and v are still in reference.
    node &
      values %~ ((<|) (LeafElem key value))
  go currentLevel node@(Internal {..}) =
    -- check if bit is set.
    -- if set, then extract the child and recursively call go
    -- if not set, create a child, modify the data structures accordingly after inserting
    if isChildPresent
    then whenChildPresent
    else whenChildNotPresent
    where
      nextLevel = currentLevel - 1
      idxIntoBitmap = idxIntoBitmapForPos currentLevel h
      nodeWithIdxSet =
        node &
          bitmap %~ setBitBM idxIntoBitmap
      isChildPresent = checkBit _bitmap idxIntoBitmap
      whenChildPresent =
        -- get the node by using popcount and modify it to accomodate the new data
        nodeWithIdxSet &
          orderedChildren.ix positionInOrderedChildren %~ go nextLevel
      whenChildNotPresent =
        nodeWithIdxSet &
          orderedChildren %~ Seq.insertAt positionInOrderedChildren (go nextLevel (empty currentLevel))
      -- Tells the index where the node has to be present
      -- Since the bitmap is stored with the least significant bits at the right end,
      -- popCountToRightOf tells the position(taking care of 0-indexing) of where to update/create.
      positionInOrderedChildren = popCountToRightOf idxIntoBitmap _bitmap

-- Not really used, but for tests
lookupHAMT :: (Eq k, Hashable k) => k -> HAMT k v -> Maybe v
lookupHAMT key root = go 5 root
  where
  h = lookupHash key
  go _ node@(Leaf _) =
    _v <$> findOf (values.traversed) (has (k.only key)) node
  go currentLevel (Internal {..}) =
    -- check if bit is set.
    -- if set, then extract the child and recursively call go
    -- if not set, create a child, modify the data structures accordingly after inserting
    if isChildPresent
    then whenChildPresent
    else Nothing
    where
      nextLevel = currentLevel - 1
      idxIntoBitmap = idxIntoBitmapForPos currentLevel h
      isChildPresent = checkBit _bitmap idxIntoBitmap
      whenChildPresent =
        -- get the node by using popcount and modify it to accomodate the new data
        go nextLevel (Seq.index _orderedChildren positionInOrderedChildren)
      -- Tells the index where the node has to be present
      -- Since the bitmap is stored with the least significant bits at the right end,
      -- popCountToRightOf tells the position(taking care of 0-indexing) of where to update/create.
      positionInOrderedChildren = popCountToRightOf idxIntoBitmap _bitmap

--Smol and friends
data Smol (version :: Nat) = Smol
  { _lookupBuffer :: !ByteString
  , _leavesBuffer :: ByteString } deriving (Show, Eq)

$(makeLenses ''Smol)

instance Serialize (Smol 1) where
  get = do
    identifier <- getBytes 4
    guard (identifier == "SMOL")
    version <- getWord8
    guard (toInteger version == 1) -- Right now only Version 1
    _lookupBuffer <- getLengthEncodedBS
    _leavesBuffer <- getLengthEncodedBS
    return $! Smol {_lookupBuffer, _leavesBuffer}

  put (Smol {..}) =
    putByteString "SMOL" >>
    putWord8 1 >> -- Version
    putLengthEncodedBS _lookupBuffer >>
    putLengthEncodedBS _leavesBuffer

data S = S
  { _leavesBuilder :: Builder
  , _count :: Word32 -- Count is useful for deserializeHAMT to find number of leaves
  , _offset :: Int }

$(makeLenses ''S)

--Serialize all children and get the bytestrings. Calculate the length of each, and adjust offsets
-- This serialize/deserializeHAMT is not considering headers

serializeHAMT :: forall k v. (Serialize k, Serialize v) => HAMT k v -> Smol 1
serializeHAMT hamt =
  Smol
    { _lookupBuffer
    , _leavesBuffer = LBS.toStrict $ B.toLazyByteString _leavesBuilder }
  where
  (_lookupBuffer, S{..}) =
    runState (go hamt) (S mempty 0 0)
  go = \case
    node@(Leaf _) -> do
      o <- _VarLength . fromIntegral <$> use offset
      let
        serializedLeaf = runPut $
          putWord8 1 >> put o
        lbs = runPutLazy $ putListOf' put (node ^. values)
        l = LBS.length lbs
      leavesBuilder <>= B.lazyByteString lbs
      count += 1
      offset += fromIntegral l
      return serializedLeaf
    node@(Internal {..}) -> do
      serializedChildren <- for (node ^. orderedChildren) go
      let
        offsets :|> _ =
          Seq.scanl (+) 0 $
            BS.length <$> serializedChildren
        maxOffset = maximum offsets
        (offsetsPut, !numWordsPerOffset) =
          if maxOffset < (fromEnum (maxBound @Word8))
          then (mapM_ (putWord8 . toEnum) offsets, 1)
          else if maxOffset < (fromEnum (maxBound @Word16))
          then (mapM_ (putWord16be . toEnum) offsets, 2)
          else if maxOffset < (fromEnum (maxBound @Word32))
          then (mapM_ (putWord32be . toEnum) offsets, 4)
          else (mapM_ (putWord64be . toEnum) offsets, 8)
        serializedNode = runPut $
          put (0 :: Word8) >>
            putNested
              (putWord16be . toEnum)
              (put _bitmap >>
               putWord8 numWordsPerOffset >>
               offsetsPut) >>
            mapM_ putByteString serializedChildren
      return serializedNode

deserializeHAMT :: forall k v. (DeSer k, DeSer v) => Smol 1 -> Either DeSerError [(k, v)]
deserializeHAMT (Smol {_leavesBuffer}) =
  runDeSer (deserManyTillEnd (deserMany @(LeafElem k v) deser)) _leavesBuffer
  <&> concat
  <&> fmap (\(LeafElem k0 v0) -> (k0, v0))

{-# INLINE lookupEncodedHamt #-}
lookupEncodedHamt
  :: forall k v
  . (Eq k, Hashable k, DeSer k, DeSer v)
  => k
  -> Smol 1
  -> Either DeSerError (Maybe v)
lookupEncodedHamt !key !smol =
  runDeSer (go 5) (smol ^. lookupBuffer) >>=
    maybe
      (return Nothing)
      getValueFromLeafAtOffset
  where
  !h = lookupHash key
  {-# INLINE findInLeaves #-}
  findInLeaves o =
    DeSer.skip (fromIntegral o) >>
    fmap _v . find (has (k.only key)) <$> DeSer.deserListOf (deser @(LeafElem k v))
  {-# INLINE getValueFromLeafAtOffset #-}
  getValueFromLeafAtOffset o =
    runDeSer
      (findInLeaves o)
      (smol ^. leavesBuffer)
  {-# INLINE go #-}
  go !currentLevel = deser @Word8 >>= \case
    1 ->
       deser @VarLength <&> Just . _unVarLength
    0 ->
      let
        nextLevel = currentLevel - 1
        !idxIntoBitmap = idxIntoBitmapForPos currentLevel h
        {-# INLINE getOffset #-}
        getOffset = do
          DeSer.skip (sizeOf (undefined :: Word16)) -- 2 bytes indicating the size of this block
          !bm <- deser
          !sizeOfOffset <- fromEnum <$> deser @Word8
          let
            isChildPresent = checkBit bm idxIntoBitmap
            offset = popCountToRightOf idxIntoBitmap bm
          if isChildPresent
          then -- get the offset
            DeSer.skip (offset * sizeOfOffset) >>
            Just <$> deSerFromSize sizeOfOffset
          else return Nothing
      in
        DeSer.lookAhead getOffset >>=
          maybe
            (return Nothing)
            (\(!offset) -> do
              deser @(Word16 @ BigE) >>= DeSer.skip . fromEnum
              DeSer.skip (fromEnum offset) >> go nextLevel
            )
    i -> throwError $ InvalidEncoding (show i)

{-# INLINE deSerFromSize #-}
deSerFromSize :: MonadDeSer m => Int -> m Int
deSerFromSize sizeOfOffset =
  if sizeOfOffset == 1
  then deser @Word8 <&> fromIntegral
  else if sizeOfOffset == 2
  then deser @(Word16 @ BigE) <&> fromIntegral
  else if sizeOfOffset == 4
  then deser @(Word16 @ BigE) <&> fromIntegral
  else deser @(Word64 @ BigE) <&> fromIntegral

{-# INLINE putListOf' #-}
putListOf' :: Putter a -> Putter [a]
putListOf' pa = \xs ->
  put (_VarLength (fromIntegral $ length xs)) >>
  mapM_ pa xs

{-# INLINE getListOf' #-}
getListOf' :: Get a -> Get [a]
getListOf' g = do
  !l <- fromIntegral . _unVarLength <$> getVarLength
  replicateM l g
