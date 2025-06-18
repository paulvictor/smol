{-# LANGUAGE OverloadedLists #-}
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

import Control.Monad.Trans.Class
import Data.Semigroup
import Data.Word
import Data.Serialize
import Control.Lens.Operators
import Control.Lens.Combinators
import Numeric.Lens
import Data.Bits
import Data.Hashable
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.VarLength
import Data.Foldable
import Data.Traversable
import Foreign.Storable (sizeOf)
import Data.Tree (drawTree, Tree(..))
import GHC.TypeLits
import Control.Monad
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.State.Strict (StateT, runStateT, State, runState)

-- bitmap is a vector of 256 bits
newtype Bitmap = Bitmap { _unBitmap :: Word64 } deriving (Eq)

instance Show Bitmap where
  show (Bitmap bm) =
    "Ox" <> (bm ^. re hex)

instance Serialize Bitmap where
  put (Bitmap bm) = putWord64be bm
  get = Bitmap <$> getWord64be

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

-- Used for construction
data LeafElem k v =
  LeafElem
  {
    _k :: k
  , _v :: v
  } deriving (Eq, Show, Functor, Foldable, Traversable)
$(makeLenses ''LeafElem)

instance (Serialize k, Serialize v) => Serialize (LeafElem k v) where
  put (LeafElem {_k, _v}) =
    putTwoOf varLengthEncoded varLengthEncoded (_k, _v)
  get =
    uncurry LeafElem <$> getTwoOf getLengthEncoded getLengthEncoded

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
fromKVPairs = foldr (\(k, v) hamt -> consHAMT k v hamt) (empty 5)

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
  , _leavesCount :: Word32
  , _leavesBuffer :: ByteString }

$(makeLenses ''Smol)

instance Serialize (Smol 1) where
  get = do
    identifier <- getBytes 4
    guard (identifier == "SMOL")
    version <- getWord8
    guard (toInteger version == 1) -- Right now only Version 1
    _lookupBuffer <- getLengthEncodedBS
    _leavesCount <- getWord32be
    _leavesBuffer <- getLengthEncodedBS
    return $! Smol {_lookupBuffer, _leavesCount, _leavesBuffer}

  put (Smol {..}) =
    putByteString "SMOL" >>
    putWord8 1 >>
    putLengthEncodedBS _lookupBuffer >>
    putWord32be _leavesCount >>
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
    , _leavesBuffer = LBS.toStrict $ B.toLazyByteString _leavesBuilder
    , _leavesCount = _count }
  where
  (_lookupBuffer, S{..}) =
    runState (go hamt) (S mempty 0 0)
  go = \case
    node@(Leaf _) -> do
      o <- use offset
      let
        serializedLeaf = runPut $
          putWord8 1 >> putWord32be (fromIntegral o)
        lbs = runPutLazy $ putListOf put (node ^. values)
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
        serializedNode = runPut $
          put (0 :: Word8) >>
            putNested
              (putWord16be . toEnum)
              (put _bitmap >>
               for_ offsets (putWord32be . fromIntegral)) >>
            mapM_ putByteString serializedChildren
      return serializedNode

deserializeHAMT :: forall k v. (Serialize k, Serialize v) => Smol 1 -> Either String [(k, v)]
deserializeHAMT (Smol {_leavesCount, _leavesBuffer}) =
  runGet
    (replicateM (fromIntegral _leavesCount) (getListOf get :: Get [LeafElem k v]))
    _leavesBuffer
  <&> concat
  <&> fmap (\(LeafElem k v)  -> (k, v))

lookupEncodedHamt :: (Eq k, Hashable k, Serialize k, Serialize v) => k -> Smol 1 -> Either String (Maybe v)
lookupEncodedHamt key smol =
  runGet (go 5) (smol ^. lookupBuffer) >>=
    maybe
      (return Nothing)
      getValueFromLeafAtOffset
  where
  !h = lookupHash key
  {-# INLINE findInLeaves #-}
  findInLeaves o =
    skip (fromIntegral o) >>
    fmap _v . find (has (k.only key)) <$> getListOf get
  {-# INLINE getValueFromLeafAtOffset #-}
  getValueFromLeafAtOffset o = runGet (findInLeaves o) (smol ^. leavesBuffer)
  go !currentLevel = getWord8 >>= \case
    1 ->
       getWord32be <&> Just
    0 ->
      let
        nextLevel = currentLevel - 1
        idxIntoBitmap = idxIntoBitmapForPos currentLevel h
        getOffset = do
          skip (sizeOf (undefined :: Word16)) -- 2 bytes indicating the size of this block
          !bm <- get
          let
            isChildPresent = checkBit bm idxIntoBitmap
            offset = popCountToRightOf idxIntoBitmap bm
          if isChildPresent
          then -- get the offset
            skip (offset * (sizeOf (undefined :: Word32))) >>
            Just <$> getWord32be
          else return Nothing
      in lookAhead getOffset >>=
        maybe
          (return Nothing)
          (\(!offset) -> do
            getWord16be >>= skip . fromEnum -- Skip this block
            skip (fromEnum offset) >> go nextLevel)
    _ -> fail "Cannot match a leaf or an internal node"
