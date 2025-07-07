{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Foldable
import Data.Functor
import Data.Bits
import Data.Serialize
import qualified Data.Serialize as Ser
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Main
import Data.Smol.JSON
import Data.Smol
import Data.VarLength
import Debug.Trace
import JSON
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM

import qualified Ser as SerTests

genHAMT ::MonadGen m => Range Int -> m (HAMT Key Value)
genHAMT = fmap fromKVPairs . genKValuePairs

main :: IO ()
main = do
  defaultMain [ SerTests.tests >> tests ]

tests :: IO Bool
tests =
  checkParallel $$(discover)

prop_SerializePreserves :: Property
prop_SerializePreserves =
  property $ do
    kvs <- forAll (genKValuePairs ( Range.constantFrom 20 1 200))
    let
      smol = serializeHAMT $ fromKVPairs kvs
      Right kvs' = deserializeHAMT @Key @Value smol
      equality = KM.fromList kvs == KM.fromList kvs'
    assert equality

prop_VarLengthEncodingWorks :: Property
prop_VarLengthEncodingWorks =
  property $ do
    w <- VarLength <$> forAll (Gen.enum 0 (1 .<<. 20))
    let
      equality = runGet getVarLength (runPut (put w)) == Right w
    assert equality

prop_LookupWorks :: Property
prop_LookupWorks =
  property $ do
    kvs <- forAll (genKValuePairs ( Range.constantFrom 20 1 200))
    let
      keys = kvs <&> fst
      bs = Ser.encode $ serializeHAMT $ fromKVPairs kvs
      Right smol1 = Ser.decode @(Smol 1) bs
    k <- forAll (Gen.element keys)
    let
      Right maybeValue = lookupEncodedHamt k smol1
      equality = maybeValue == (snd <$> find ((== k). fst) kvs)
    assert equality
