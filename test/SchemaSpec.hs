{-# OPTIONS_GHC -fprint-potential-instances #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchemaSpec (spec, main) where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy
import Data.Functor.Identity
import Data.Proxy
import Data.Schematic
import Data.Schematic.Generator
import Data.Singletons
import Data.Tagged
import Data.Vinyl
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck as SC
import Test.SmallCheck.Drivers as SC
import Test.SmallCheck.Series as SC

import Debug.Trace

type SchemaExample = 'SchemaObject
  '[ '("foo", 'SchemaArray '[ 'AEq 1] ('SchemaNumber '[ 'NGt 10]))
   , '("bar", 'SchemaOptional ('SchemaText '[ 'TEnum '["foo", "bar"]]))]

type SchemaExample2 = 'SchemaObject
  '[ '("foo", 'SchemaArray '[ 'AEq 2] ('SchemaText '[ 'TGt 10]))
   , '("bar", 'SchemaOptional ('SchemaText '[ 'TRegex "[0-9]+"]))]

jsonExample :: JsonRepr SchemaExample
jsonExample = withRepr @SchemaExample
   $ field @"bar" (Just "bar")
  :& field @"foo" [12]
  :& RNil

type AddQuuz =
  'Migration "add_field_quuz"
   '[ 'Diff '[] ('AddKey "quuz" (SchemaNumber '[])) ]

type DeleteQuuz =
  'Migration "remove_field_quuz"
    '[ 'Diff '[] ( 'DeleteKey "quuz") ]

type SwapFields =
  'Migration "swap_fields"
    '[ 'Diff '[ 'PKey "bar" ] ('Update
       ('SchemaArray '[ 'AEq 1] ('SchemaNumber '[ 'NGt 10])))
     , 'Diff '[ 'PKey "foo" ] ('Update
       ('SchemaOptional ('SchemaText '[ 'TEnum '["foo", "bar"]]))) ]

type Migrations = '[ AddQuuz
                   , DeleteQuuz ]
                   -- , SwapFields ]

type VersionedJson = 'Versioned SchemaExample Migrations

migrationList :: MigrationList Identity VersionedJson
migrationList
  =   (migrateObject (\r -> Identity $ field @"quuz" 42 :& r))
  :&& shrinkObject
  -- :&& (migrateObject (\r -> Identity
  --     $  field @"foo" (r ^. flens (Proxy @"bar") . _Just . optionalRepr)
  --     :& field @"bar" (r ^. flens (Proxy @"foo") . arrayRepr)
  --     :& RNil))
  :&& MNil

schemaJson :: ByteString
schemaJson = "{\"foo\": [13], \"bar\": null}"

schemaJson2 :: ByteString
schemaJson2 = "{\"foo\": [3], \"bar\": null}"

schemaJsonSeries :: Monad m => SC.Series m (JsonRepr SchemaExample)
schemaJsonSeries = series

schemaJsonSeries2 :: Monad m => SC.Series m (JsonRepr SchemaExample2)
schemaJsonSeries2 = series

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
      (Proxy @VersionedJson)
      migrationList
      schemaJson
        `shouldSatisfy` isValid
  it "validates json series" $ property $
    SC.over schemaJsonSeries $ \x ->
      isValid (parseAndValidateJson @SchemaExample (toJSON x))
  it "validates json series 2" $ property $
    SC.over schemaJsonSeries2 $ \x ->
      isValid (parseAndValidateJson @SchemaExample2 (toJSON x))


main :: IO ()
main = hspec spec
