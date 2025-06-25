module JSON where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

genNull :: MonadGen m => m Value
genNull = return Null

genString :: MonadGen m => m Value
genString = String <$> Gen.text (Range.constant 1 100) Gen.alphaNum

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
  genKeyMap = Object . KM.fromList <$> (genKValuePairs (Range.constantFrom 10 1 20))

genKValuePairs :: MonadGen m => Range Int -> m [(Key, Value)]
genKValuePairs range = Gen.list range ((,) <$> genKey <*> genValue)
  where
  genKey = Key.fromText <$> Gen.text (Range.constant 1 100) Gen.alphaNum
