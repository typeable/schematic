{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonSchemaSpec (spec, main) where

import Data.Aeson as J
import Data.Maybe
import Data.Proxy
import Data.ByteString.Lazy as B
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

type SchemaArrayExample = 'SchemaObject
  '[ '("a1", 'SchemaArray '[ 'AGt 1 ] ('SchemaNumber '[]))
  ,  '("a2", 'SchemaArray '[ 'AGe 1 ] ('SchemaNumber '[]))
  ,  '("a3", 'SchemaArray '[ 'ALt 1 ] ('SchemaNumber '[]))
  ,  '("a4", 'SchemaArray '[ 'ALe 1 ] ('SchemaNumber '[]))
  ]

type SchemaNumberExample = 'SchemaObject
  '[ '("n1", 'SchemaNumber '[ 'NGt 1 ])
  ,  '("n2", 'SchemaNumber '[ 'NGe 1 ])
  ,  '("n3", 'SchemaNumber '[ 'NLt 1 ])
  ,  '("n4", 'SchemaNumber '[ 'NLe 1 ])
  ]

type SchemaTextExample = 'SchemaObject
  '[ '("t1", 'SchemaText '[ 'TGt 1 ])
  ,  '("t2", 'SchemaText '[ 'TGe 1 ])
  ,  '("t3", 'SchemaText '[ 'TLt 1 ])
  ,  '("t4", 'SchemaText '[ 'TLe 1 ])
  ]

spec :: Spec
spec = do
  it "validates simple schema" $ do
    let schema = D4.SchemaWithURI (fromJust $ toJsonSchema (Proxy @SchemaExample)) Nothing
    fetchHTTPAndValidate schema (toJSON exampleData) >>= \case
      Left _ -> fail "failed to validate test example"
      Right _ -> pure ()
  it "validates schema with arrays" $ do
    let
      schema = D4.SchemaWithURI (fromJust $ toJsonSchema (Proxy @SchemaArrayExample)) Nothing
      obj = withRepr @SchemaArrayExample $
           field @"a1" [ReprNumber 13, ReprNumber 13]
        :& field @"a2" [ReprNumber 13]
        :& field @"a3" []
        :& field @"a4" [ReprNumber 13]
        :& RNil
    fetchHTTPAndValidate schema (toJSON obj) >>= \case
      Left e -> fail "failed to validate test example"
      Right _ -> pure ()
  it "validates schema with numbers" $ do
    let
      schema = D4.SchemaWithURI (fromJust $ toJsonSchema (Proxy @SchemaNumberExample)) Nothing
      obj = withRepr @SchemaNumberExample $
           field @"n1" 2
        :& field @"n2" 1
        :& field @"n3" 0
        :& field @"n4" 1
        :& RNil
    fetchHTTPAndValidate schema (toJSON obj) >>= \case
      Left e -> fail "failed to validate test example"
      Right _ -> pure ()
  it "validates schema with strings" $ do
    let
      schema = D4.SchemaWithURI (fromJust $ toJsonSchema (Proxy @SchemaTextExample)) Nothing
      obj = withRepr @SchemaTextExample $
           field @"t1" "11"
        :& field @"t2" "1"
        :& field @"t3" ""
        :& field @"t4" "1"
        :& RNil
    fetchHTTPAndValidate schema (toJSON obj) >>= \case
      Left e -> fail "failed to validate test example"
      Right _ -> pure ()

main :: IO ()
main = hspec spec
