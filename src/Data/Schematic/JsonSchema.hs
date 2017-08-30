{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Data.Schematic.JsonSchema where

import           Data.Aeson
import qualified Data.Map as M
import           Data.Schematic.Schema
import           Data.Text


draft6 :: Text
draft6 = "http://json-schema.org/draft-06/schema#"

data JsonSchemaType
  = JObject
  | JArray
  | JNumber
  | JString
  | JNull

instance ToJSON JsonSchemaType where
  toJSON JObject = "object"
  toJSON JArray = "array"
  toJSON JNumber = "number"
  toJSON JString = "string"
  toJSON JNull = "null"

data JsonSchema
  = RootSchema
    { type_ :: JsonSchemaType
    , title :: Text
    , items :: Maybe Text
    , schemaDraft :: Text }
  | JsonObject
    { type_ :: JsonSchemaType
    , properties :: M.Map Text JsonSchema
    , additionalProperties :: Bool
    , required :: [Text]
    , schemaDraft :: Text }
  | JsonOptional JsonSchema

jsonObject :: M.Map Text JsonSchema -> JsonSchema
jsonObject m = JsonObject
  { type_ = JObject
  , properties = m
  , additionalProperties = False
  , required = M.keys $ M.filter
    (\case; JsonOptional _ -> False; _ -> True) m
  , schemaDraft = draft6 }

instance ToJSON JsonSchema where
  toJSON RootSchema{..} = object [ "type" .= type_, "title" .= title ]
  toJSON JsonObject{..} =
    object
      [ "type" .= type_
      , "properties" .= properties
      , "additionalProperties" .= additionalProperties
      , "required" .= required
      , "schemaDraft" .= schemaDraft]

-- Root schema:
--
-- {
--     "title": "root",
--     "items": {
--         "title": "array item"
--     }
-- }

-- toSchema :: JsonRepr a -> JsonSchema
-- toSchema = \case
--   ReprText t ->
--   ReprNumber n ->
--   ReprNull ->
--   ReprArray arr ->
--   ReprObject obj ->
--   ReprOptional mv ->
