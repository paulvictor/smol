{-# LANGUAGE OverloadedStrings #-}
module SerDe where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Monad.IO.Class
import Debug.Trace
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson
import Data.Bits
import Data.Word
import qualified Data.Serialize as Ser
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.VarLength
import Data.DeSer
import JSON
import Data.Smol.JSON

propDeSerW8Works :: Property
propDeSerW8Works =
  property $ do
    w8 <- forAll (Gen.word8 Range.constantBounded)
    let
      bs = Ser.encode w8
      Right w8' :: Either DeSerError Word8 = runDeSer bs
    w8 === w8'

propVarLengthDeSerWorks :: Property
propVarLengthDeSerWorks = do
  property $ do
    w <- _VarLength <$> forAll (Gen.enum 0 (1 .<<. 20))
    let
      bs = Ser.encode w
      Right w' :: Either DeSerError VarLength = runDeSer bs
    w === w'

propValueDeSerWorks :: Property
propValueDeSerWorks = do
  property $ do
    value <- forAll genValue
    let
      bs = Ser.encode value
      value' :: Value =
        either
          (String . T.pack . show)
          id
          (runDeSer bs)
    value === value'

props :: [(PropertyName, Property)]
props =
  [ ("prop_w8_serde_works", propDeSerW8Works)
  , ("prop_var_length_encoding_works", propVarLengthDeSerWorks)
  , ("prop_value_encoding_works", propValueDeSerWorks)
  ]
