{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ser where

import Data.Bits ((.<<.))
import qualified Data.VarLength as VarLength
import qualified Data.ByteString.Builder as Builder
import Data.Ser
import qualified Data.Ser as Ser
import Data.Serialize
import qualified Data.Serialize as Serialize
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog
import JSON
import Data.Aeson
import Data.Smol.JSON

prop_SerWord8Works :: Property
prop_SerWord8Works =
  property $ do
    w8 <- forAll Gen.enumBounded
    let
      BufferWithLen b l = runSerializing $ Ser.word8 w8
      lbs = Builder.toLazyByteString b
      Right w8' = Serialize.runGetLazy Serialize.getWord8 lbs
    assert (l == 1)
    assert (w8 == w8')

prop_SerWord16Works :: Property
prop_SerWord16Works =
  property $ do
    w16 <- forAll Gen.enumBounded
    let
      BufferWithLen b l = runSerializing $ Ser.word16BE w16
      lbs = Builder.toLazyByteString b
      Right w16' = Serialize.runGetLazy Serialize.getWord16be lbs
    assert (l == 2)
    assert (w16 == w16')

prop_SerWord32Works :: Property
prop_SerWord32Works =
  property $ do
    w32 <- forAll Gen.enumBounded
    let
      BufferWithLen b l = runSerializing $ Ser.word32BE w32
      lbs = Builder.toLazyByteString b
      Right w32' = Serialize.runGetLazy Serialize.getWord32be lbs
    assert (l == 4)
    assert (w32 == w32')

prop_SerWord64Works :: Property
prop_SerWord64Works =
  property $ do
    w64 <- forAll $ Gen.word64 Range.constantBounded
    let
      BufferWithLen b l = runSerializing $ Ser.word64BE w64
      lbs = Builder.toLazyByteString b
      Right w64' = Serialize.runGetLazy Serialize.getWord64be lbs
    assert (l == 8)
    assert (w64 == w64')

prop_SerIntWorks :: Property
prop_SerIntWorks =
  property $ do
    i <- forAll $ Gen.int Range.constantBounded
    let
      BufferWithLen b l = runSerializing $ Ser.ser i
      lbs = Builder.toLazyByteString b
      Right i' = fromIntegral <$> Serialize.runGetLazy Serialize.getInt64be lbs
    assert (l == 8)
    assert (i == i')

prop_SerVarLengthWorks :: Property
prop_SerVarLengthWorks =
  property $ do
    i <- forAll $ Gen.word $ Range.constant 0 (1 .<<. 20)
    let
      BufferWithLen b l = runSerializing $ Ser.varLength (fromIntegral i)
      lbs = Builder.toLazyByteString b
      Right i' = VarLength._unVarLength <$> Serialize.runGetLazy VarLength.getVarLength lbs
    assert (i == i')

prop_SerBSWorks :: Property
prop_SerBSWorks =
  property $ do
    src <- forAll $ Gen.bytes (Range.constant 0 2000)
    let
      BufferWithLen b l = runSerializing $ Ser.ser src
      lbs = Builder.toLazyByteString b
      Right dst = Serialize.runGetLazy VarLength.getLengthEncodedBS lbs
    assert (src == dst)

prop_SerValueWorks :: Property
prop_SerValueWorks =
  property $ do
    src <- forAll genValue
    let
      BufferWithLen b l = runSerializing $ Ser.ser src
      lbs = Builder.toLazyByteString b
      Right dst = Serialize.runGetLazy (get @Value)  lbs
    assert (src == dst)

tests :: IO Bool
tests =
  checkParallel $$(discover)

