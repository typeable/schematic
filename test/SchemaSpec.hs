{-# OPTIONS_GHC -fprint-potential-instances #-}

module SchemaSpec (spec, main) where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Functor.Identity
import Data.Proxy
import Data.Schematic
import Data.Vinyl
import Test.Hspec


type SchemaExample
  = 'SchemaObject
    '[ '("foo", 'SchemaArray '[ 'AEq 1] ('SchemaNumber '[ 'NGt 10]))
     , '("bar", 'SchemaOptional ('SchemaText '[ 'TEnum '["foo", "bar"]]))]

type TestMigration =
  'Migration "test_revision"
    '[ 'Diff '[ 'PKey "bar" ] ('Update ('SchemaText '[]))
     , 'Diff '[ 'PKey "foo" ] ('Update ('SchemaNumber '[])) ]

type VS = 'Versioned SchemaExample '[ TestMigration ]

jsonExample :: JsonRepr SchemaExample
jsonExample = ReprObject $
  FieldRepr (ReprArray [ReprNumber 12])
    :& FieldRepr (ReprOptional (Just (ReprText "bar")))
    :& RNil

schemaJson :: ByteString
schemaJson = "{\"foo\": [13], \"bar\": null}"

schemaJson2 :: ByteString
schemaJson2 = "{\"foo\": [3], \"bar\": null}"

topObject
  :: JsonRepr
    ('SchemaObject
      '[ '("foo", 'SchemaNumber '[])
       , '("bar", 'SchemaText '[])])
topObject = ReprObject $
  FieldRepr (ReprNumber 42)
    :& FieldRepr (ReprText "test")
    :& RNil

spec :: Spec
spec = do
  -- it "show/read JsonRepr properly" $
  --   read (show example) == example
  it "decode/encode JsonRepr properly" $
    decode (encode jsonExample) == Just jsonExample
  it "validates correct representation" $
    ((decodeAndValidateJson schemaJson) :: ParseResult (JsonRepr SchemaExample))
      `shouldSatisfy` isValid
  it "returns decoding error on structurally incorrect input" $
    ((decodeAndValidateJson "{}") :: ParseResult (JsonRepr SchemaExample))
      `shouldSatisfy` isDecodingError
  it "validates incorrect representation" $
    ((decodeAndValidateJson schemaJson2) :: ParseResult (JsonRepr SchemaExample))
      `shouldSatisfy` isValidationError
  -- it "validates versioned json" $ do
  --   decodeAndValidateVersionedJson (Proxy @VS) schemaJson
  --     `shouldSatisfy` isValid
  it "validates versioned json with a migration list" $ do
    decodeAndValidateVersionedWithPureMList
      (Proxy @VS)
      ((:&&)
        (Proxy @(SchemaByRevision "test_revision" VS))
        (const $ Identity topObject) MNil)
      schemaJson
        `shouldSatisfy` isValid

main :: IO ()
main = hspec spec
