{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Data.Schematic.JsonSchema where

import           Data.Aeson
import qualified Data.Map as M
import           Data.Schematic.Schema
import           Data.Singletons
import           Data.Text
import qualified JSONSchema.Draft4.Schema as D4


draft4 :: Text
draft4 = "http://json-schema.org/draft-04/schema#"

-- toJsonSchema :: JsonRepr schema -> D4.Schema
-- toJsonSchema (ReprText t) = D4.emptySchema
--   { D4._schemaVersion = pure draft4
--   }

toJsonSchema
  :: forall schema. (SingI schema)
  => Sing (schema :: Schema)
  -> D4.Schema
toJsonSchema _ = case fromSing (sing :: Sing schema) of
  SchemaText tcs -> D4.emptySchema
  SchemaNumber ncs -> D4.emptySchema
  SchemaObject ocs -> D4.emptySchema
  SchemaArray acs sch -> D4.emptySchema
  SchemaNull -> D4.emptySchema
  SchemaOptional sch -> D4.emptySchema

-- jsonObject :: M.Map Text JsonSchema -> JsonSchema
-- jsonObject m = JsonObject
--   { type_ = JObject
--   , properties = m
--   , additionalProperties = False
--   , required = M.keys $ M.filter
--     (\case; JsonOptional _ -> False; _ -> True) m
--   , schemaDraft = draft6 }

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
