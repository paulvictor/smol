{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Bits
import Data.Serialize
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Main
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Smol.JSON
import Data.Smol
import Data.VarLength

genNull :: MonadGen m => m Value
genNull = return Null

genString :: MonadGen m => m Value
genString = String <$> Gen.text (Range.constant 1 20) Gen.alphaNum

genNumber :: MonadGen m => m Value
genNumber =
  Gen.choice [integrals]--, doubles
  where
    integrals = Number . fromIntegral <$> Gen.int (Range.constantBounded)
--     doubles = Number . (realToFrac @Double) <$> Gen.realFrac_ (Range.constantFrom 0 (-10000) 10000)

genBool :: MonadGen m => m Value
genBool = Bool <$> Gen.bool

genValue :: MonadGen m => m Value
genValue =
  Gen.recursive Gen.choice [genNull, genString, genNumber, genBool] [genArray, genKeyMap]
  where
  genArray = Array . V.fromList <$> Gen.list (Range.constant 0 10) genValue
  genKeyMap = Object . KM.fromList <$> genKValuePairs

genKValuePairs :: MonadGen m => m [(Key, Value)]
genKValuePairs = Gen.list (Range.constant 0 10) ((,) <$> genKey <*> genValue)
  where
  genKey = Key.fromText <$> Gen.text (Range.constant 1 20) Gen.alphaNum

main :: IO ()
main = do
  defaultMain [tests]

tests :: IO Bool
tests =
  checkParallel $
    Group "Data.Smol"
    [ ("prop_var_length_encoding_works", propVarLengthEncodingWorks)
    , ("prop_serialize_preserves", propSerializePreserves)
    ]

propSerializePreserves :: Property
propSerializePreserves =
  property $ do
    kvs <- forAll genKValuePairs
    let
      smol = serializeHAMT $ fromKV kvs
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

