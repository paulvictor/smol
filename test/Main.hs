{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified SerDe as SerDe
import Data.Foldable
import Data.Functor
import Data.Bits
import Data.Serialize
import qualified Data.Serialize as Ser
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Main
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Smol
import Data.VarLength
import Debug.Trace
import Data.DeSer
import JSON

genHAMT ::MonadGen m => Range Int -> m (HAMT Key Value)
genHAMT = fmap fromKVPairs . genKValuePairs

main :: IO ()
main = do
  defaultMain [tests]

tests :: IO Bool
tests =
  checkSequential $ Group "Deser" SerDe.props
--   checkParallel $
--     Group "Data.Smol"
--     [ ("prop_var_length_encoding_works", propVarLengthEncodingWorks)
--     , ("prop_lookup_works", propLookupWorks)
--     , ("prop_serialize_preserves", propSerializePreserves)
--     ]

propSerializePreserves :: Property
propSerializePreserves =
  property $ do
    kvs <- forAll (genKValuePairs ( Range.constantFrom 20 1 200))
    let
      smol = serializeHAMT $ fromKVPairs kvs
      Right kvs' = deserializeHAMT @Key @Value smol
      equality = KM.fromList kvs == KM.fromList kvs'
    assert equality

propVarLengthEncodingWorks :: Property
propVarLengthEncodingWorks =
  property $ do
    w <- _VarLength <$> forAll (Gen.enum 0 (1 .<<. 20))
    let
      equality = runGet getVarLength (runPut (put w)) == Right w
    assert equality

propLookupWorks :: Property
propLookupWorks =
  property $ do
    kvs <- forAll (genKValuePairs ( Range.constantFrom 20 1 200))
    let
      keys = kvs <&> fst
      bs = Ser.encode $ serializeHAMT $ fromKVPairs kvs
      Right smol = Ser.decode @(Smol 1) bs
    k <- forAll (Gen.element keys)
    let
      Right maybeValue = lookupEncodedHamt k smol
      equality = maybeValue == (snd <$> find ((== k). fst) kvs)
    assert equality
