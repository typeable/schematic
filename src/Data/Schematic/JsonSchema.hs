{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}

module Data.Schematic.JsonSchema
  ( toJsonSchema
  ) where

import Control.Monad.State.Strict
import Data.Foldable
import Data.HashMap.Strict as H
import Data.List.NonEmpty
import Data.Schematic.Schema as S
import Data.Singletons
import Data.Text
import JSONSchema.Draft4.Schema as D4
import JSONSchema.Validator.Draft4 as D4


draft4 :: Text
draft4 = "http://Jason-schema.org/draft-04/schema#"

-- FIXME: implement all this later
textConstraint :: DemotedTextConstraint -> State D4.Schema ()
textConstraint (DTEq _) = pure ()
textConstraint (DTLt _) = pure ()
textConstraint (DTLe _) = pure ()
textConstraint (DTGt _) = pure ()
textConstraint (DTGe _) = pure ()
textConstraint (DTRegex _) = pure ()
textConstraint (DTEnum _) = pure ()

numberConstraint :: DemotedNumberConstraint -> State D4.Schema ()
numberConstraint (DNLe _) = pure ()
numberConstraint (DNLt _) = pure ()
numberConstraint (DNGt _) = pure ()
numberConstraint (DNGe _) = pure ()
numberConstraint (DNEq _) = pure ()

arrayConstraint :: DemotedArrayConstraint -> State D4.Schema ()
arrayConstraint (DAEq _) = pure ()

toJsonSchema
  :: forall proxy schema
   . SingI schema
  => proxy (schema :: S.Schema)
  -> D4.Schema
toJsonSchema _ = (toJsonSchema' $ fromSing (sing :: Sing schema))
  { _schemaVersion = pure draft4 }

toJsonSchema'
  :: DemotedSchema
  -> D4.Schema
toJsonSchema' = \case
  DSchemaText tcs ->
    execState (traverse_ textConstraint tcs) $ emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaString }
  DSchemaNumber ncs ->
    execState (traverse_ numberConstraint ncs) $ emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaNumber }
  DSchemaBoolean -> emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaBoolean }
  DSchemaObject objs -> emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaObject
      , _schemaProperties = pure $ H.fromList $ (\(n,s) -> (n, toJsonSchema' s))
        <$> objs }
  DSchemaArray acs sch ->
    execState (traverse_ arrayConstraint acs) $ emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaArray
      , _schemaItems = pure $ ItemsArray [toJsonSchema' sch] }
  DSchemaNull -> emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaNull }
  DSchemaOptional sch -> emptySchema
      { _schemaOneOf = pure $ toJsonSchema' DSchemaNull :| [toJsonSchema' sch] }
