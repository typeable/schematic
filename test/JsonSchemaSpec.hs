module JsonSchemaSpec (spec, main) where

import Data.Aeson as J
import Data.Maybe
import Data.Proxy
import Data.Schematic
import Data.Vinyl
import JSONSchema.Draft4 as D4
import Test.Hspec


type ArraySchema = 'SchemaArray '[ 'AEq 1] ('SchemaNumber '[ 'NGt 10])

type ArrayField = '("foo", ArraySchema)

type FieldsSchema =
  '[ ArrayField, '("bar", 'SchemaOptional ('SchemaText '[ 'TEnum '["foo", "bar"]]))]

type SchemaExample = 'SchemaObject FieldsSchema

exampleData :: JsonRepr SchemaExample
exampleData = withRepr @SchemaExample $
     field @"foo" [ReprNumber 13]
  :& field @"bar" (pure (ReprText "foo"))
  :& RNil

spec :: Spec
spec = do
  it "validates simple schema" $ do
    let schema = D4.SchemaWithURI (fromJust $ toJsonSchema (Proxy @SchemaExample)) Nothing
    fetchHTTPAndValidate schema (toJSON exampleData) >>= \case
      Left _ -> fail "failed to validate test example"
      Right _ -> pure ()

main :: IO ()
main = hspec spec
