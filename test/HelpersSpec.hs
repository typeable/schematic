{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module HelpersSpec (spec, main) where

import Control.Lens
import Data.Aeson
import Data.ByteString.Char8 as C8
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Lens
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import Data.Proxy
import Data.Schematic
import Data.Text as T
import Data.Text.Lens
import Data.Vinyl
import Test.Hspec


type UUIDSchema = 'SchemaObject '[ '("uuid", 'SchemaText IsUUID) ]

-- | Taken from UUID RFC 4122
uuid :: Text
uuid = "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"

type ISO8601Schema = 'SchemaObject '[ '("datetime", 'SchemaText IsDateTime) ]

-- | Taken from ISO8601 RFC 3339
iso8601datetimes :: [Text]
iso8601datetimes =
  [ "1985-04-12T23:20:50.52Z"
  , "1996-12-19T16:39:57-08:00"
  , "1990-12-31T23:59:60Z"
  , "1990-12-31T15:59:60-08:00"
  , "1937-01-01T12:00:27.87+00:20"
  ]

spec :: Spec
spec = do
  describe "UUID: " $ do
    it "validates correct UUID" $ do
      let
        uuidJson = "{\"uuid\": \"" <> (uuid ^. unpacked . packedChars) <> "\" }"
      (decodeAndValidateJson uuidJson :: ParseResult (JsonRepr UUIDSchema))
        `shouldSatisfy` isValid

    it "fails to validate incorrect UUID" $ do
      let
        incorrectJson = "{\"uuid\": \"incorrect\" }"
      (decodeAndValidateJson incorrectJson :: ParseResult (JsonRepr UUIDSchema))
        `shouldSatisfy` isValidationError

  describe "ISO8601 datetime: " $ do
    for_ iso8601datetimes $ \dt -> do
      it ("validates correct datetime - " <> dt ^. unpacked) $ do
        let json = "{\"datetime\": \"" <> (dt ^. unpacked . packedChars) <> "\" }"
        (decodeAndValidateJson json :: ParseResult (JsonRepr ISO8601Schema))
          `shouldSatisfy` isValidationError

main :: IO ()
main = hspec spec
