module LensSpec (spec, main) where

import Control.Lens
import Data.Kind
import Data.Proxy
import Data.Schematic
import Data.Vinyl
import Test.Hspec


type ArraySchema = 'SchemaArray '[AEq 1] ('SchemaNumber '[NGt 10])

type ArrayField = '("foo", ArraySchema)

type FieldsSchema =
  '[ ArrayField, '("bar", 'SchemaOptional ('SchemaText '[TEnum '["foo", "bar"]]))]

type SchemaExample = 'SchemaObject FieldsSchema

arrayData :: JsonRepr ArraySchema
arrayData = ReprArray [ReprNumber 13]

arrayField :: FieldRepr ArrayField
arrayField = FieldRepr arrayData

objectData :: Rec FieldRepr FieldsSchema
objectData = FieldRepr arrayData
  :& FieldRepr (ReprOptional (Just (ReprText "foo")))
  :& RNil

exampleData :: JsonRepr SchemaExample
exampleData = ReprObject objectData

spec :: Spec
spec = do
  let
    newFooVal = FieldRepr $ ReprArray [ReprNumber 15]
    fooProxy  = Proxy @"foo"
  it "gets the field from an object" $ do
    fget fooProxy objectData == arrayField
  it "sets the object field" $ do
    fget fooProxy (fput newFooVal objectData) == newFooVal

  describe "(using lens library) " $ do
    it "get the field from an object" $ do
      objectData ^. flens (Proxy @"foo") == arrayField
    it "sets the object field" $ do
      set (flens (Proxy @"foo")) newFooVal objectData ^. flens (Proxy @"foo")
        == newFooVal

main :: IO ()
main = hspec spec
