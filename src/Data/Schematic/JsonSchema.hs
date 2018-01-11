{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Schematic.JsonSchema
  ( toJsonSchema
  , toJsonSchema'
  ) where

import Control.Monad.State.Strict
import Data.Aeson as J
import Data.Foldable as F
import Data.HashMap.Strict as H
import Data.List.NonEmpty as NE
import Data.Schematic.Schema as S
import Data.Singletons
import Data.Text
import Data.Traversable
import JSONSchema.Draft4.Schema as D4
import JSONSchema.Validator.Draft4 as D4


draft4 :: Text
draft4 = "http://json-schema.org/draft-04/schema#"

textConstraint :: DemotedTextConstraint -> State D4.Schema ()
textConstraint (DTEq n) = modify $ \s -> s
  { _schemaMinLength = pure $ fromIntegral n
  , _schemaMaxLength = pure $ fromIntegral n }
textConstraint (DTLt n) = modify $ \s -> s
  { _schemaMaxLength = pure . fromIntegral $ n + 1 }
textConstraint (DTLe n) = modify $ \s -> s
  { _schemaMaxLength = pure . fromIntegral $ n }
textConstraint (DTGt n) =
  let n' = if n == 0 then 0 else n - 1
  in modify $ \s -> s { _schemaMinLength = pure . fromIntegral $ n' }
textConstraint (DTGe n) = modify $ \s -> s
  { _schemaMinLength = pure . fromIntegral $ n }
textConstraint (DTRegex r) = modify $ \s -> s { _schemaPattern = pure r }
textConstraint (DTEnum ss) =
  let ss' = if F.length ss == 0 then [] else NE.fromList $ J.String <$> ss
  in modify $ \s -> s { _schemaEnum = pure ss' }

numberConstraint :: DemotedNumberConstraint -> State D4.Schema ()
numberConstraint (DNLe n) = modify $ \s -> s
  { _schemaMaximum = pure . fromIntegral $ n }
numberConstraint (DNLt n) = modify $ \s -> s
  { _schemaMaximum = pure . fromIntegral $ n + 1 }
numberConstraint (DNGt n) = modify $ \s -> s
  { _schemaMinimum = pure . fromIntegral $ n }
numberConstraint (DNGe n) =
  let n' = if n == 0 then 0 else n - 1
  in modify $ \s -> s { _schemaMinimum = pure . fromIntegral $ n' }
numberConstraint (DNEq n) = modify $ \s -> s
  { _schemaMinimum = pure $ fromIntegral n
  , _schemaMaximum = pure $ fromIntegral n }

arrayConstraint :: DemotedArrayConstraint -> State D4.Schema ()
arrayConstraint (DAEq _) = pure ()

toJsonSchema
  :: forall proxy schema
   . SingI schema
  => proxy (schema :: S.Schema)
  -> Maybe D4.Schema
toJsonSchema _ = do
  js <- toJsonSchema' $ fromSing (sing :: Sing schema)
  pure $ js { _schemaVersion = pure draft4 }

toJsonSchema'
  :: DemotedSchema
  -> Maybe D4.Schema
toJsonSchema' = \case
  DSchemaText tcs ->
    pure $ execState (traverse_ textConstraint tcs) $ emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaString }
  DSchemaNumber ncs ->
    pure $ execState (traverse_ numberConstraint ncs) $ emptySchema
      { _schemaType = pure $ TypeValidatorString D4.SchemaNumber }
  DSchemaBoolean -> pure $ emptySchema
    { _schemaType = pure $ TypeValidatorString D4.SchemaBoolean }
  DSchemaObject objs -> do
    res <- for objs $ \(n,s) -> do
      s' <- toJsonSchema' s
      pure (n, s')
    pure $ emptySchema
      { _schemaType       = pure $ TypeValidatorString D4.SchemaObject
      , _schemaProperties = pure $ H.fromList res }
  DSchemaArray acs sch -> do
    res <- toJsonSchema' sch
    pure $ execState (traverse_ arrayConstraint acs) $ emptySchema
      { _schemaType  = pure $ TypeValidatorString D4.SchemaArray
      , _schemaItems = pure $ ItemsObject res }
  DSchemaNull -> pure $ emptySchema
    { _schemaType = pure $ TypeValidatorString D4.SchemaNull }
  DSchemaOptional sch -> do
    snull <- toJsonSchema' DSchemaNull
    sres <- toJsonSchema' sch
    pure $ emptySchema { _schemaOneOf = pure (snull :| [sres]) }
  DSchemaUnion sch -> do
    schemaUnion <- traverse toJsonSchema' sch >>= \case
      [] -> Nothing
      x  -> Just x
    pure $ emptySchema { _schemaAnyOf = pure $ NE.fromList schemaUnion }
