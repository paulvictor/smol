{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main(main) where

import Data.Functor
import Data.Traversable
import Data.Conduit
import Data.Conduit.Combinators (stdin, stdout, linesUnboundedAscii)
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Zstd as Zstd
import qualified Data.Conduit.List as C (mapMaybe)
import Data.Aeson
import Data.Smol
import Data.Smol.JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Serialize as Ser
import Data.Serialize (get)
import Data.Conduit.Cereal
import qualified Data.Text as T
import Options.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS

optionsParser :: Parser [Key]
optionsParser =
  some $
    strOption $
      (long "key"
      <> short 'k'
      <> metavar "KEY")

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

main :: IO ()
main =
  convert
--   execParser (info optionsParser mempty) >>= lookup_

lookup_ :: [Key] -> IO ()
lookup_ keys =
  runConduit $
    stdin
    .| Zstd.decompress
    .| conduitGet2 get
    .| C.map (\smol1 -> KM.fromList (keys <&> (\key -> (key, join $ hush (lookupEncodedHamt @Key @Value key smol1)))))
    .| C.map encode
    .| C.mapM_ (LBS.putStrLn)

convert :: IO ()
convert =
  runConduit $
    stdin
    .| Zstd.decompress
    .| linesUnboundedAscii
    .| C.mapMaybe (decodeStrict @Object)
    .| C.map (fromKVPairs . KM.toList)
    .| C.map trimHAMT
    .| C.map (Ser.encode . serializeHAMT)
    .| Zstd.compress 4
    .| stdout
