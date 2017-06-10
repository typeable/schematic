{-# OPTIONS_GHC -fprint-potential-instances #-}

module SchemaSpec (spec, main) where

import Control.Monad
import Data.ByteString.Lazy
import Data.Aeson
import Data.Proxy
import Data.Schematic
import Data.Singletons
import Data.Vinyl
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series.Instances


type SchemaExample
  = SchemaObject
    '[ '("foo", SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
     , '("bar", SchemaOptional (SchemaText '[TEnum '["foo", "bar"]]))]

type VS =
  'Versioned SchemaExample
    '[ 'Migration "test_revision"
       '[ 'Diff '[ 'PKey "bar" ] ('Update ('SchemaText '[]))
        , 'Diff '[ 'PKey "foo" ] ('Update ('SchemaNumber '[])) ] ]

exampleTest :: JsonRepr (SchemaOptional (SchemaText '[TEq 3]))
exampleTest = ReprOptional (Just (ReprText "lil"))

exampleNumber :: JsonRepr (SchemaNumber '[NGt 10])
exampleNumber = ReprNumber 12

exampleArray :: JsonRepr (SchemaArray '[AEq 1] (SchemaNumber '[NGt 10]))
exampleArray = ReprArray [exampleNumber]

jsonExample :: JsonRepr SchemaExample
jsonExample = ReprObject $
  FieldRepr (ReprArray [ReprNumber 12])
    :& FieldRepr (ReprOptional (Just (ReprText "bar")))
    :& RNil

schemaJson :: ByteString
schemaJson = "{\"foo\": [13], \"bar\": null}"

schemaJson2 :: ByteString
schemaJson2 = "{\"foo\": [3], \"bar\": null}"

schemaJsonTopVersion :: ByteString
schemaJsonTopVersion = "{ \"foo\": 42, \"bar\": \"bar\" }"

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
  it "validates versioned json" $ do
    (decodeAndValidateVersionedJson (Proxy @VS) schemaJsonTopVersion)
      `shouldSatisfy` isValid

main :: IO ()
main = hspec spec
